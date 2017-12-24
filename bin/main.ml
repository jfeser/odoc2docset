open Printf

open Bos
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
      <key>dashIndexFilePath</key>
      <string>index.html</string>
      <key>DashDocSetFamily</key>
      <string>dashtoc</string>
      </dict>
      </plist>
    |}
    pkg_name pkg_name pkg_name

let process_pkg db name unit docu_dir =
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

  let add_anchor id type_ name_ =
    let anchor_elem =
      Soup.create_element
        ~attributes:["name", sprintf "//apple_ref/cpp/%s/%s" type_ name_]
        ~class_:"dashAnchor"
        "a"
    in
    let open DocOckHtml in
    let file, anchor =
      match Url.from_identifier ~get_package:(fun _ -> name) ~stop_before:false id with
      | Ok { page; anchor; kind } ->
        let file =
          let html_path =
            List.rev ("index.html" :: page)
            |> String.concat "/"
            |> Fpath.of_string |> ok_exn
          in
          Fpath.(docu_dir // html_path)
        in
        (file, anchor)
      | Error e -> failwith (Url.Error.to_string e)
    in
    if anchor = "" || not (OS.File.exists file |> ok_exn) then () else
      let soup = OS.File.read file |> ok_exn |> Soup.parse in
      begin match Soup.select_one (sprintf {|a[href="#%s"]|} anchor) soup with
        | Some node -> Soup.prepend_child node anchor_elem
        | None -> eprintf "Warning: Could not find anchor node for %s in %s.\n" anchor (Fpath.to_string file)
      end;
      OS.File.write file (Soup.to_string soup) |> ok_exn
  in

  let module Id = Paths.Identifier in
  let rec name : 'a Id.any -> string =
    function
    | Id.Root (_, x)
    | Id.Page (_, x)
    | Id.CoreType x
    | Id.CoreException x -> x
    | Id.Module (p, x)
    | Id.ModuleType (p, x)
    | Id.Type (p, x)
    | Id.Extension (p, x)
    | Id.Exception (p, x)
    | Id.Value (p, x)
    | Id.Class (p, x)
    | Id.ClassType (p, x) -> (name (Id.any p)) ^ "." ^ x
    | Id.Method (p, x)
    | Id.InstanceVariable (p, x) -> (name (Id.any p)) ^ "#" ^ x
    | Id.Label _
    | Id.Constructor _
    | Id.Argument _
    | Id.Field _ -> failwith "No printable path."
  in

  let index id type_ =
    insert db (name (any id)) type_ (url id);
    add_anchor id type_ (Paths.Identifier.name id)
  in

  let open Types in
  let nop = fun _ -> () in
  let rec process_module_decl =
    let open ModuleType in
    let rec process_module_type_expr = function
      | Signature s -> process_signature s
      | Functor (_, x) -> process_module_type_expr x
      | _ -> ()
    in
    let open Module in
    function
    | Alias _ -> ()
    | ModuleType e -> process_module_type_expr e
  and process_module Module.({ id; type_; expansion; hidden }) =
    let open Module in
    if not hidden then begin
      index id "Module";
      process_module_decl type_;
      begin match expansion with
        | Some (Signature s)
        | Some (Functor (_, s)) -> process_signature s
        | _ -> ()
      end
    end
  and process_module_type ModuleType.({ id }) = index id "Interface"
  and process_type_ext = nop
  and process_type TypeDecl.({ id }) = index id "Type"
  and process_exception Exception.({ id }) = index id "Exception"
  and process_val id type_ =
    let open TypeExpr in
    let kind = match type_ with
      | Arrow _ -> "Function"
      | Object _ -> "Object"
      | _ -> "Value"
    in
    index id kind
  and process_value Value.({ id; type_ }) = process_val id type_
  and process_external External.({ id; type_ }) = process_val id type_
  and process_method Method.({ id; private_ }) =
    if not private_ then index id "Method"
  and process_instance_variable InstanceVariable.({ id }) = index id "Variable"
  and process_class_signature ClassSignature.({ items }) =
    let open ClassSignature in
    List.iter (function
        | Method x -> process_method x
        | InstanceVariable x -> process_instance_variable x
        | _ -> ()) items
  and process_class Class.({ id; type_ }) =
    let open Class in
    let rec process_decl = function
      | ClassType (Signature x) -> process_class_signature x
      | ClassType (Constr _) -> ()
      | Arrow (_, _, d) -> process_decl d
    in
    process_decl type_;
    index id "Class"
  and process_class_type = nop
  and process_include Include.({ expansion = { content } }) =
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
  and process_signature sig_ = List.iter process_signature_item sig_
  and process_unit Unit.({ id; content }) =
    let process_packed items =
      List.iter (fun Unit.Packed.({ id; path }) -> index id "Module") items
    in
    match content with
    | Module x -> index id "Module"; process_signature x
    | Pack x -> process_packed x
  in
  process_unit unit

let main output_path pkg_names =
  (* Create the docset template. *)
  let docset_dir = Fpath.of_string output_path |> ok_exn in
  let docset_name = Fpath.(rem_ext docset_dir |> basename) in
  let res_dir = Fpath.(docset_dir / "Contents" / "Resources") in
  let docu_dir = Fpath.(res_dir / "Documents") in
  OS.Dir.create docu_dir |> ok_exn |> ignore;
  OS.File.write Fpath.(docset_dir / "Contents" / "Info.plist") (plist docset_name) |> ok_exn;
  let etc_dir = Fpath.of_string Opam_config.etc |> ok_exn in
  OS.Cmd.(Cmd.(v "cp" % Fpath.(etc_dir / "icon.png" |> to_string) % Fpath.(docset_dir / "icon.png" |> to_string)) |> run_status ~quiet:true) |> ok_exn |> ignore;

  (* Generate documentation using Odoc. *)
  eprintf "Running odoc..."; flush stderr;
  let _ =
    OS.Cmd.(Cmd.(v "odig" % "odoc" %% of_list pkg_names) |> run_status ~quiet:true) |> ok_exn in
  eprintf " done.\n";
  let conf = Odig.Conf.of_opam_switch () |> ok_exn in
  let doc_dir = Odig.Odoc.htmldir conf None in

  (* Copy documentation. *)
  eprintf "Copying documentation..."; flush stderr;
  Sys.command (sprintf "cp -r %s/* %s"
                 Fpath.(to_string doc_dir)
                 Fpath.(docu_dir |> to_string)) |> ignore;
  eprintf " done.\n";

  (* Create index db. *)
  let db_file = Fpath.(res_dir / "docSet.dsidx") in
  OS.Path.delete db_file |> ok_exn;
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
  eprintf "Indexing..."; flush stderr;
  pkgs |> Odig.Pkg.Set.iter (fun pkg ->
      let name = Odig.Pkg.name pkg in
      eprintf " %s," name; flush stderr;

      insert db name "Package" (sprintf "%s/index.html" name);

      let cachedir = Odig.Pkg.cachedir pkg in
      OS.Dir.contents cachedir
      |> ok_exn
      |> List.iter (fun f ->
          if Fpath.has_ext "odoc" f then
            let unit =
              Odoc.Unit.load (Odoc.Fs.File.of_string (Fpath.to_string f))
            in
            let env = Odoc.Env.build builder (`Unit unit) in
            let expander = Odoc.Env.expander env in
            let unit = try DocOck.expand expander unit with _ -> unit in
            process_pkg db name unit docu_dir));
  eprintf " done.\n"

let () =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: odoc2docset DOCSET PKG..."
  else
    main
      Sys.argv.(1)
      (Array.sub Sys.argv 2 (Array.length Sys.argv - 2) |> Array.to_list)
