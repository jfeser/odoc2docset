open Printf

open DocOck

let read_file cmi =
  match read_cmi (fun name _ -> name) cmi with
  | Not_an_interface -> failwith "not an interface"
  | Wrong_version -> failwith "wrong OCaml version"
  | Corrupted -> failwith "corrupted"
  | Not_a_typedtree -> failwith "not a typedtree"
  | Not_an_implementation -> failwith "not an implementation"
  | Ok intf -> intf

let insert db name typ path =
  let query = sprintf "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('%s', '%s', '%s');" name typ path
  in
  (* printf "%s\n" query; *)
  ignore (Sqlite3.exec db query)

let ok_exn : ('a, [`Msg of string]) Pervasives.result -> 'a = function
  | Ok x -> x
  | Error (`Msg e) -> failwith e

let plist pkg_name =
  sprintf
    {|<?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
	    <key>CFBundleIdentifier</key>
	    <string>%s</string>
	    <key>CFBundleName</key>
	    <string>%s</string>
	    <key>DocSetPlatformFamily</key>
	    <string>%s</string>
	    <key>isDashDocset</key>
	    <true/>
      </dict>
      </plist>
    |}
    pkg_name pkg_name pkg_name

let main pkg_name =
  (* Generate documentation using Odoc. *)
  (* Sys.command ("odig odoc") |> ignore; *)
  let conf = Odig.Conf.of_opam_switch () |> ok_exn in
  let doc_dir = Odig.Odoc.htmldir conf None in

  (* Create the docset template. *)
  let docset_dir = Fpath.v (sprintf "%s.docset" pkg_name) in
  let res_dir = Fpath.(docset_dir / "Contents" / "Resources") in
  Bos.OS.Dir.create Fpath.(res_dir / "Documents") |> ok_exn |> ignore;
  Bos.OS.File.write Fpath.(docset_dir / "Contents" / "Info.plist") (plist pkg_name) |> ok_exn;

  (* Copy documentation. *)
  Sys.command (sprintf "cp -r %s/* %s"
                 (Fpath.to_string doc_dir)
                 Fpath.(res_dir / "Documents" |> to_string)) |> ignore;

  (* Create index db. *)
  let db_file = Fpath.(res_dir / "docSet.dsidx" |> to_string) in
  let db = Sqlite3.db_open db_file in
  ignore (Sqlite3.exec db "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);");
  ignore (Sqlite3.exec db "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);
");

  (* Populate index. *)
  let pkgs = Odig.Pkg.set conf |> ok_exn in
  Odig.Pkg.Set.iter (fun pkg ->
      let name = Odig.Pkg.name pkg in
      printf "Processing %s.\n" name;
      flush stdout;

      insert db name "Package" (sprintf "%s/index.html" name);

      let url id =
        let open DocOckHtml in
        match Url.from_identifier ~get_package:(fun _ -> name) ~stop_before:false id with
        | Ok { page; anchor; kind } ->
          List.rev ("index.html" :: page)
          |> String.concat "/"
          |> (fun path -> if anchor = "" then path else path ^ "#" ^ anchor)
        | Error e -> failwith (Url.Error.to_string e)
      in
      let nop = fun _ _ -> () in
      let process_module = fun db Types.Module.({ id; }) ->
        let name = Paths.Identifier.name id in
        insert db name "Module" (url id)
      in
      let process_module_type = nop in
      let process_type_ext = nop in
      let process_type = fun db Types.TypeDecl.({ id; }) ->
        let name = Paths.Identifier.name id in
        insert db name "Type" (url id)
      in
      let process_exception = fun db Types.Exception.({ id; }) ->
        let name = Paths.Identifier.name id in
        insert db name "Exception" (url id)
      in
      let process_value = fun db Types.Value.({ id; type_ }) ->
        let name = Paths.Identifier.name id in
        let kind = match type_ with
          | Arrow _ -> "Function"
          | Object _ -> "Object"
          | _ -> "Value"
        in
        insert db name kind (url id)
      in
      let process_external = nop in
      let process_class = fun db Types.Class.({ id; }) ->
        let name = Paths.Identifier.name id in
        insert db name "Class" (url id)
      in
      let process_class_type = nop in
      let process_include = nop in

      let process_signature_item =
        let open Types.Signature in
        fun db -> function
          | Module x -> process_module db x
          | ModuleType x -> process_module_type db x
          | Type x -> process_type db x
          | TypExt x -> process_type_ext db x
          | Exception x -> process_exception db x
          | Value x -> process_value db x
          | External x -> process_external db x
          | Class x -> process_class db x
          | ClassType x -> process_class_type db x
          | Include x -> process_include db x
          | Comment x -> ()
      in

      let process_signature : Sqlite3.db -> 'a Types.Signature.t -> unit =
        fun db sig_ -> List.iter (process_signature_item db) sig_
      in

      let process_pack db x = () in

      let process_unit db Types.Unit.({ id; content }) =
        match content with
        | DocOck.Types.Unit.Module x ->
          let name = Paths.Identifier.name id in
          insert db name "Module" (url id);
          process_signature db x
        | DocOck.Types.Unit.Pack x -> process_pack db x
      in

      let cobjs = Odig.Pkg.cobjs pkg in
      List.iter (fun cmi ->
          let path = Odig.Cobj.Cmi.path cmi |> Fpath.to_string in
          try
            read_file path |> process_unit db
          with _ -> ())
        (Odig.Cobj.cmis cobjs)
    ) pkgs

let () = main "odig"
