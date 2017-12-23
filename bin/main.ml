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
  printf "%s\n" query;
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

let process_pkg db name unit =
  let any = Paths.Identifier.any in

  let url id =
    let open DocOckHtml in
    match Url.from_identifier ~get_package:(fun _ -> name) ~stop_before:false id with
    | Ok { page; anchor; kind } ->
      List.rev ("index.html" :: page)
      |> String.concat "/"
      |> (fun path -> if anchor = "" then path else path ^ "#" ^ anchor)
    | Error e -> failwith (Url.Error.to_string e)
  in

  let rec name =
    let open Paths.Identifier in
    function
    | Root (_, x)
    | Page (_, x)
    | CoreType x
    | CoreException x -> x
    | Module (p, x)
    | ModuleType (p, x)
    | Type (p, x)
    | Extension (p, x)
    | Exception (p, x)
    | Value (p, x)
    | Class (p, x)
    | ClassType (p, x) -> (name p) ^ "." ^ x
    | Method (p, x)
    | InstanceVariable (p, x) -> (name p) ^ "#" ^ x
    | Label _
    | Constructor _
    | Argument _
    | Field _ -> failwith "No printable path."
  in
  let nop = fun _ -> () in

  let open Types in

  let rec process_module_decl =
    let open ModuleType in
    let rec process_module_type_expr = function
      | Signature s -> process_signature s
      | Functor (_, x) -> process_module_type_expr x
      | _ -> ()
    in
    let open Module in
    function
    | Alias p ->
      printf "Alias %s" (Paths.string_of_sexp (Paths.Path.sexp_of_t (fun _ -> Atom "") p))
    | ModuleType e -> process_module_type_expr e
  and process_module = fun Module.({ id; type_; expansion; hidden }) ->
    let open Module in
    if not hidden then begin
      insert db (name (any id)) "Module" (url id);
      process_module_decl type_;
      begin match expansion with
        | Some (Signature s)
        | Some (Functor (_, s)) -> process_signature s
        | _ -> ()
      end
    end
  and process_module_type = fun ModuleType.({ id }) ->
    let open ModuleType in
    insert db (name (any id)) "Interface" (url id)
  and process_type_ext = nop
  and process_type = fun TypeDecl.({ id; }) ->
    insert db (name (any id)) "Type" (url id)
  and process_exception = fun Exception.({ id; }) ->
    insert db (name (any id)) "Exception" (url id)
  and process_val id type_ =
    let open TypeExpr in
    let kind = match type_ with
      | Arrow _ -> "Function"
      | Object _ -> "Object"
      | _ -> "Value"
    in
    insert db (name (any id)) kind (url id)
  and process_value = fun Value.({ id; type_ }) ->
    process_val id type_
  and process_external External.({ id; type_ }) =
    process_val id type_
  and process_method Method.({ id; private_ }) =
    if not private_ then
      insert db (name (any id)) "Method" (url id)
  and process_instance_variable InstanceVariable.({ id; }) =
    insert db (name (any id)) "Variable" (url id)
  and process_class_signature ClassSignature.({ items }) =
    let open ClassSignature in
    List.iter (function
        | Method x -> process_method x
        | InstanceVariable x -> process_instance_variable x
        | _ -> ()) items
  and process_class = fun Class.({ id; type_ }) ->
    let open Class in
    let open ClassType in
    let rec process_decl = function
      | ClassType (Signature x) -> process_class_signature x
      | ClassType (Constr _) -> ()
      | Arrow (_, _, d) -> process_decl d
    in
    process_decl type_;
    insert db (name (any id)) "Class" (url id)
  and process_class_type = nop
  and process_include Include.({ expansion = { content } }) =
    let open Include in
    process_signature content
  and process_signature_item =
    let open Signature in
    function
      | Module x -> process_module x
      | ModuleType x -> process_module_type x
      | Type x -> process_type x
      | TypExt x -> process_type_ext x
      | Exception x -> process_exception x
      | Value x -> process_value x
      | External x -> process_external x
      | Class x -> process_class x
      | ClassType x -> process_class_type x
      | Include x -> process_include x
      | Comment x -> ()
  and process_signature sig_ =
    List.iter process_signature_item sig_
  and process_unit Unit.({ id; content }) =
    let open Unit in
    let process_packed items =
      let open Packed in
      List.iter (fun { id; path } ->
          insert db (name (any id)) "Module" (url id)) items
    in
    match content with
    | Module x ->
      insert db (name (any id)) "Module" (url id);
      process_signature x
    | Pack x -> process_packed x
  in

  process_unit unit

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
  eprintf "Copying documentation...";
  Sys.command (sprintf "cp -r %s/* %s"
                 (Fpath.to_string doc_dir)
                 Fpath.(res_dir / "Documents" |> to_string)) |> ignore;
  eprintf " done.\n";
  flush stderr;

  (* Create index db. *)
  let db_file = Fpath.(res_dir / "docSet.dsidx") in
  Bos.OS.Path.delete db_file |> ok_exn;
  let db = Sqlite3.db_open (Fpath.to_string db_file) in
  ignore (Sqlite3.exec db "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);");
  ignore (Sqlite3.exec db "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);");

  let pkgs = Odig.Pkg.set conf |> ok_exn in

  (* Collect include directories. *)
  let include_dirs =
    Odig.Pkg.Set.fold (fun pkg dirs ->
        Odig.Pkg.libdir pkg :: Odig.Pkg.cachedir pkg :: dirs) pkgs []
    |> List.map (fun d -> Odoc.Fs.Directory.of_string (Fpath.to_string d))
  in
  let builder =
    Odoc.Env.create ~important_digests:true ~directories:include_dirs
  in

  (* Populate index. *)
  pkgs
  |> Odig.Pkg.Set.iter (fun pkg ->
      let name = Odig.Pkg.name pkg in
      eprintf "Processing %s.\n" name;
      flush stderr;

      insert db name "Package" (sprintf "%s/index.html" name);

      let cachedir = Odig.Pkg.cachedir pkg in
      Bos.OS.Dir.contents cachedir
      |> ok_exn
      |> List.iter (fun f ->
          if Fpath.has_ext "odoc" f then
            let unit =
              Odoc.Unit.load (Odoc.Fs.File.of_string (Fpath.to_string f))
            in
            let env = Odoc.Env.build builder (`Unit unit) in
            let resolver = Odoc.Env.resolver env in
            let expander = Odoc.Env.expander env in
            (* let unit = try DocOck.resolve resolver unit with _ -> unit in *)
            let unit = try DocOck.expand expander unit with _ -> unit in
            process_pkg db name unit))

let () = main "odig"
