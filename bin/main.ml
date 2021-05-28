open Core
open Shell
open Cmdliner
module Odoc = Odoc_odoc
module Model = Odoc_model
module Html = Odoc_html
module Url = Odoc_document.Url

let ( / ) = Filename.concat

let run_warn cmd args =
  try run cmd args
  with Process.Failed r -> Logs.warn (fun m -> m "%s" @@ Process.format_failed r)

let run_lines_warn cmd args =
  try run_lines cmd args
  with Process.Failed r ->
    Logs.warn (fun m -> m "%s" @@ Process.format_failed r);
    []

let ok_exn = function
  | Ok x -> x
  | Error (`Msg e) ->
      Logs.err (fun m -> m "%s" e);
      Caml.exit 1

let opam_prefix = Sys.getenv_exn "OPAM_SWITCH_PREFIX"

module Odig = struct
  let htmldir pkg = opam_prefix / "var/cache/odig/html" / pkg

  let cachedir = opam_prefix / "var/cache/odig/odoc"

  let all_pkgs () = run_lines "odig" [ "pkg"; "--short" ]

  let theme_prefix = opam_prefix / "share/odig/odoc-theme"
end

module Sqlite3 = struct
  module Rc = struct
    include Sqlite3.Rc

    let ok_exn =
      let open Sqlite3.Rc in
      function
      | OK -> ()
      | e ->
          Logs.err (fun m -> m "Sqlite error: %s" (to_string e));
          Caml.exit 1

    let done_exn =
      let open Sqlite3.Rc in
      function
      | DONE -> ()
      | e ->
          Logs.err (fun m -> m "Sqlite error: %s" (to_string e));
          Caml.exit 1
  end

  include (Sqlite3 : module type of Sqlite3 with module Rc := Rc)
end

let insert db name typ path =
  let query =
    sprintf
      "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('%s', '%s', \
       '%s');"
      name typ path
  in
  Sqlite3.Rc.check @@ Sqlite3.exec db query

let tar =
  (* Prefer GNU tar to BSD tar. *)
  run_first_line_exn "which" [ "gtar"; "tar" ]

let etc = opam_prefix / "etc/odoc2docset"

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

let id_to_string id =
  let open Odoc_model in
  let open Paths.Identifier in
  let open Names in
  let exception Not_printable in
  let rec i2s : Paths.Identifier.t -> _ = function
    | `Root _ | `Page _ | `RootPage _ | `LeafPage _ | `Parameter _ | `Result _ -> ""
    | `CoreType x ->
        if TypeName.is_hidden x then raise Not_printable else TypeName.to_string x
    | `CoreException x -> ExceptionName.to_string x
    | `Module (p, x) ->
        if ModuleName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ ModuleName.to_string x
    | `ModuleType (p, x) ->
        if ModuleTypeName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ ModuleTypeName.to_string x
    | `Type (p, x) ->
        if TypeName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ TypeName.to_string x
    | `Extension (p, x) -> i2s (p :> t) ^ "." ^ ExtensionName.to_string x
    | `Exception (p, x) -> i2s (p :> t) ^ "." ^ ExceptionName.to_string x
    | `Value (p, x) ->
        if ValueName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ ValueName.to_string x
    | `Class (p, x) ->
        if ClassName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ ClassName.to_string x
    | `ClassType (p, x) ->
        if ClassTypeName.is_hidden x then raise Not_printable
        else i2s (p :> t) ^ "." ^ ClassTypeName.to_string x
    | `Method (p, x) -> i2s (p :> t) ^ "#" ^ MethodName.to_string x
    | `InstanceVariable (p, x) ->
        i2s (p :> t) ^ "#" ^ InstanceVariableName.to_string x
    | `Constructor (p, x) -> (
        match p with
        | `Type (p, _) -> i2s (p :> t) ^ "." ^ ConstructorName.to_string x
        | `CoreType _ -> ConstructorName.to_string x)
    | `Field (p, x) -> (
        match p with
        | `Root _ | `Module _ | `ModuleType _ | `Type _ | `CoreType _ | `Parameter _
        | `Result _ ->
            i2s (p :> t) ^ "." ^ FieldName.to_string x
        | `Class _ | `ClassType _ -> i2s (p :> t) ^ "#" ^ FieldName.to_string x)
    | `Label _ -> raise Not_printable
  in
  try Some (i2s id) with Not_printable -> None

let id_kind (id : Odoc_model.Paths.Identifier.t) =
  match id with
  | `Type _ | `CoreType _ -> "Type"
  | `InstanceVariable _ -> "Variable"
  | `Parameter _ | `Result _ | `Module _ -> "Module"
  | `ModuleType _ -> "Interface"
  | `Method _ -> "Method"
  | `Field _ -> "Field"
  | `Label _ -> "Parameter"
  | `Exception _ | `CoreException _ -> "Exception"
  | `Class _ -> "Class"
  | `RootPage _ | `Page _ | `LeafPage _ -> "Section"
  | `ClassType _ -> "Class"
  | `Value _ -> "Value"
  | `Constructor _ -> "Constructor"
  | `Extension _ -> "Extension"
  | `Root _ -> "Package"

let is_hidden id =
  Option.map (id_to_string id) ~f:(String.is_substring ~substring:"__")
  |> Option.value ~default:true

(** Collect the identifiers (labeled with Dash types) in a compilation unit. *)
let ids_of_unit = Ids.compilation_unit

let update_index db docu_dir ids =
  let open Sqlite3 in
  let stmt =
    prepare db "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?,?,?);"
  in
  exec db "BEGIN TRANSACTION;" |> Rc.ok_exn;
  let soups = Hashtbl.Poly.create () in
  (* Filter out the ids that don't have a string representation. *)
  List.filter_map ids ~f:(fun id ->
      match id_to_string id with
      | Some id_str -> Some (id, id_kind id, id_str)
      | None -> None)
  (* Filter out hidden ids. *)
  |> List.filter ~f:(fun (id, _, _) -> not (is_hidden id))
  (* Filter out the ids that don't have a URL. *)
  |> List.filter_map ~f:(fun (id, type_, id_str) ->
         match Url.from_identifier ~stop_before:false id with
         | Ok url -> Some (id, type_, id_str, url)
         | Error e ->
             Logs.warn (fun m ->
                 m "Failed to get URL for id '%s': %s" id_str (Url.Error.to_string e));
             None)
  (* Filter out the ids that don't have a corresponding documentation file. *)
  |> List.filter_map ~f:(fun (id, type_, id_str, url) ->
         let file =
           let html_path =
             Odoc_html.Link.href ~resolve:(Base "") { url with anchor = "" }
           in
           sprintf "%s/%s" docu_dir html_path
         in
         match Sys.file_exists file with
         | `Yes -> Some (id, type_, id_str, url, file)
         | `No | `Unknown ->
             Logs.debug (fun m -> m "Documentation file %s does not exist." file);
             None)
  (* Filter out the ids that have an anchor that does not exist in the
     documentation file. *)
  |> List.filter_map ~f:(fun (id, type_, id_str, url, file) ->
         let url =
           if String.(url.Url.Anchor.anchor = "") then Some url
           else
             let soup =
               Hashtbl.find_or_add soups file ~default:(fun () ->
                   In_channel.read_all file |> Soup.parse)
             in
             let name_ = Model.Paths.Identifier.name id in
             let new_anchor = sprintf "//apple_ref/cpp/%s/%s" type_ name_ in
             let anchor_elem =
               Soup.create_element ~attributes:[ ("name", new_anchor) ]
                 ~class_:"dashAnchor" "a"
             in
             match Soup.select_one (sprintf {|a[href="#%s"]|} url.anchor) soup with
             | Some node ->
                 Soup.prepend_child node anchor_elem;
                 Some { url with anchor = new_anchor }
             | None ->
                 Logs.debug (fun m ->
                     m "Could not find anchor node for %s in %s." url.anchor file);
                 None
         in
         Option.map url ~f:(fun url -> (type_, id_str, url)))
  (* Insert ids into the database. *)
  |> List.iter ~f:(fun (type_, id_str, url) ->
         let url_str = Odoc_html.Link.href ~resolve:(Base "") url in
         Logs.debug (fun m -> m "Inserting %s %s %s" id_str type_ url_str);
         let open Data in
         bind stmt 1 (TEXT id_str) |> Rc.ok_exn;
         bind stmt 2 (TEXT type_) |> Rc.ok_exn;
         bind stmt 3 (TEXT url_str) |> Rc.ok_exn;
         step stmt |> Rc.done_exn;
         reset stmt |> Rc.ok_exn);
  exec db "END TRANSACTION;" |> Rc.ok_exn;
  Hashtbl.iteri soups ~f:(fun ~key:file ~data:soup ->
      Out_channel.write_all file ~data:(Soup.to_string soup))

let create_template docset_dir =
  let open Filename in
  let docset_name = chop_extension docset_dir |> basename in
  let resource_dir = docset_dir / "Contents" / "Resources" in
  let documents_dir = resource_dir / "Documents" in
  let db_file = resource_dir / "docSet.dsidx" in
  run "rm" [ "-rf"; docset_dir ];
  mkdir ~p:() documents_dir;
  Out_channel.write_all
    (docset_dir / "Contents" / "Info.plist")
    ~data:(plist docset_name);
  run_warn "cp" [ etc / "icon.png"; docset_dir / "icon.png" ];
  (docset_dir, documents_dir, db_file)

let create_db db_file =
  let db = Sqlite3.db_open db_file in
  Sqlite3.exec db
    "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path \
     TEXT);"
  |> Sqlite3.Rc.ok_exn;
  Sqlite3.exec db "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);"
  |> Sqlite3.Rc.ok_exn;
  db

(** Load a compilation unit, resolve and expand it. Taken straight from
   odoc/src/html_page.ml. *)
let load_unit env path =
  let open Result.Let_syntax in
  let open Odoc in
  let%bind unit = Compilation_unit.load (Fs.File.of_string path) in
  let env = Env.build env (`Unit unit) in
  Odoc_xref2.Link.link env unit
  |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error:true ~filename:path

let load env input =
  let open Odoc in
  let open Or_error in
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ -> Ok None
  | Compilation_unit { hidden; _ } ->
      if hidden then Ok None
      else
        Compilation_unit.load input >>= fun unit ->
        let env = Env.build env (`Unit unit) in
        let linked = Odoc_xref2.Link.link env unit in
        linked
        |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error:false
             ~filename:input_s
        >>= fun odoctree ->
        Odoc_xref2.Tools.reset_caches ();
        Caml.Hashtbl.clear Compilation_unit.units_cache;
        Ok (Some odoctree)

let odoc_files_exn d = run_lines "find" [ d; "-type"; "f"; "-name"; "*.odoc" ]

let all_subdirs d = run_lines_warn "find" [ d; "-type"; "d" ]

let populate_db include_dirs pkgs db docu_dir =
  List.iter include_dirs ~f:(fun d ->
      Logs.debug (fun m -> m "Include dir: %s" (Odoc.Fs.Directory.to_string d)));

  let env =
    Odoc.Env.create ~important_digests:true ~directories:include_dirs
      ~open_modules:[]
  in
  List.iter pkgs ~f:(fun pkg ->
      Logs.info (fun m -> m "Indexing %s." pkg);

      insert db pkg "Package" (sprintf "%s/index.html" pkg);

      List.iter
        (odoc_files_exn (Odig.cachedir / pkg))
        ~f:(fun f ->
          Logs.debug (fun m -> m "Loading %s." f);
          match load env @@ Odoc.Fs.File.of_string f with
          | Ok (Some unit) ->
              let ids = ids_of_unit unit in
              update_index db docu_dir ids
          | Ok None -> ()
          | Error (`Msg msg) -> Logs.err (fun m -> m "Resolve error: %s" msg)))

let tarix_to_sqlite tarix_fn sqlite_fn =
  let module Sql = Sqlite3 in
  (* Create new sqlite db. *)
  let db = Sql.db_open sqlite_fn in
  Sql.exec db
    "CREATE TABLE tarindex(path TEXT PRIMARY KEY COLLATE NOCASE, hash TEXT);\n\
     CREATE TABLE toextract(path TEXT PRIMARY KEY COLLATE NOCASE, hash TEXT);\n"
  |> Sql.Rc.ok_exn;
  let insert_stmt =
    Sql.prepare db "INSERT OR IGNORE INTO tarindex(path, hash) VALUES (?,?);"
  in
  Sql.(exec db "BEGIN TRANSACTION;" |> Rc.ok_exn);
  In_channel.with_file tarix_fn ~f:(fun ch ->
      (In_channel.fold_lines ch ~init:0 ~f:(fun lnum line ->
           (if lnum > 0 then
            match String.split line ~on:' ' with
            | [ kind; off1; off2; len; fn ] ->
                if String.(kind = "0") then (
                  Sql.(
                    bind insert_stmt 1 (TEXT fn) |> Rc.ok_exn;
                    bind insert_stmt 2
                      (TEXT (String.concat ~sep:" " [ off1; off2; len ]))
                    |> Rc.ok_exn;
                    step insert_stmt |> Rc.done_exn;
                    reset insert_stmt |> Rc.ok_exn))
            | _ -> Logs.warn (fun m -> m "Unexpected line in tarix file: %s" line));
           lnum + 1)
        : int)
      |> ignore);
  Sql.(exec db "END TRANSACTION;" |> Rc.ok_exn)

let compress_docset docset_dir =
  let temp_tgz = Caml.Filename.temp_file "tarix" ".tgz"
  and temp_idx = Caml.Filename.temp_file "tarixIndex" ".dbtxt"
  and temp_db = Caml.Filename.temp_file "docSet" ".dsidx"
  and db_file = docset_dir / "Contents/Resources/docSet.dsidx" in
  (* Copy out the docset index. *)
  cp db_file temp_db;
  (* Convert the tarix index to a sqlite index. *)
  let docset_base, docset_rel = Filename.split docset_dir in
  let tarix_args = sprintf "-z -9 -f %s" temp_idx in
  run
    ~env:(`Extend [ ("TARIX", tarix_args) ])
    "tar"
    [
      "-cf";
      temp_tgz;
      "--use-compress-program";
      "tarix";
      "-C";
      docset_base;
      docset_rel;
    ];
  (* Create a new docset template. *)
  rm ~r:() ~f:() docset_dir;
  (create_template docset_dir : string * string * string) |> ignore;
  (* Generate a sqlite based tar index. *)
  tarix_to_sqlite temp_idx (docset_dir / "Contents/Resources/tarixIndex.db");
  (* Copy the docset index and compressed docset back in. *)
  rm ~r:() ~f:() (docset_dir / "Contents/Resources/Documents");
  cp temp_tgz (docset_dir / "Contents/Resources/tarix.tgz");
  cp temp_db (docset_dir / "Contents/Resources/docSet.dsidx")

let main () compress theme output_path pkg_names =
  (* Get Odig configuration. *)
  let all_pkgs = Odig.all_pkgs () in

  (* Look up all the selected packages. *)
  let pkgs =
    match pkg_names with
    | [] -> all_pkgs
    | names ->
        let names =
          List.filter names ~f:(fun n ->
              if List.mem all_pkgs n ~equal:String.( = ) then true
              else (
                Logs.err (fun m -> m "Could not find package %s." n);
                false))
        in
        if Logs.err_count () > 0 then Caml.exit 1 else names
  in

  (* Create the docset template. *)
  let docset_dir, docu_dir, db_file = create_template output_path in

  (* Generate documentation using Odoc. *)
  Logs.info (fun m -> m "Running odoc.");
  run "odig" ("odoc" :: pkgs);
  Logs.info (fun m -> m "Done running odoc.");

  (* Copy documentation. *)
  Logs.info (fun m -> m "Copying documentation.");
  List.iter pkgs ~f:(fun pkg ->
      Logs.debug (fun m -> m "Copying %s." pkg);
      run "cp" [ "-r"; Odig.htmldir pkg; docu_dir ]);

  (* Copy theme CSS & JS. *)
  run "cp"
    [ "-rf"; Odig.theme_prefix / theme; Filename.concat docu_dir "_odoc-theme" ];
  cp (opam_prefix / "share/odoc/odoc-theme/default/highlight.pack.js") docu_dir;

  Logs.info (fun m -> m "Done copying documentation.");

  Logs.info (fun m -> m "Creating index.");
  let db = create_db db_file in
  let include_dirs =
    all_subdirs Odig.cachedir |> List.map ~f:Odoc.Fs.Directory.of_string
  in
  populate_db include_dirs pkgs db docu_dir;
  Logs.info (fun m -> m "Done creating index.");
  if compress then (
    Logs.info (fun m -> m "Compressing docset.");
    compress_docset docset_dir;
    Logs.info (fun m -> m "Done compressing docset."))

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let cmd =
  let setup_log =
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
  in
  let compressed =
    let doc = "Generate a compressed docset." in
    Arg.(value & flag & info [ "c"; "compress" ] ~doc)
  in
  let theme =
    let doc = "Odig theme to use." in
    Arg.(value & opt string "light" & info [ "t"; "theme" ] ~doc)
  in
  let docset_dir =
    let doc =
      "Destination of the docset. If the docset already exists, it will be \
       overwritten."
    in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DOCSET" ~doc)
  in
  let pkgs =
    let doc =
      "Packages to include in the docset. If no packages are specified, then all \
       the installed packages will be used."
    in
    Arg.(value & pos_right 0 string [] & info [] ~docv:"PKG" ~doc)
  in
  ( Term.(const main $ setup_log $ compressed $ theme $ docset_dir $ pkgs),
    Term.info "odoc2docset" )

let () = Term.(exit @@ eval cmd)
