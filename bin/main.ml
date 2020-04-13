open Base
open Printf
open Bos
open Cmdliner
open Rresult
open Stdio
module Odoc = Odoc_odoc
module Model = Odoc_model
module Html = Odoc_html
module Xref = Odoc_xref

let ok_exn = function
  | Ok x -> x
  | Error (`Msg e) ->
      Logs.err (fun m -> m "%s" e);
      Caml.exit 1

let opam_prefix = Sys.getenv_exn "OPAM_SWITCH_PREFIX" |> Fpath.of_string |> ok_exn

module Odig = struct
  let htmldir pkg = Fpath.(opam_prefix / "var" / "cache" / "odig" / "html" / pkg)

  let cachedir pkg = Fpath.(opam_prefix / "var" / "cache" / "odig" / "odoc" / pkg)

  let all_pkgs () =
    let out =
      OS.Cmd.run_out Cmd.(v "odig" % "pkg" % "--short")
      |> OS.Cmd.to_string ~trim:true |> ok_exn
    in
    String.split ~on:'\n' out

  let theme_prefix = Fpath.(opam_prefix / "share" / "odig" / "odoc-theme")
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
  ignore (Sqlite3.exec db query)

let tar =
  (* Prefer GNU tar to BSD tar. *)
  let tar_cmds = Cmd.[ v "gtar"; v "tar" ] in
  List.find_exn tar_cmds ~f:(fun c -> OS.Cmd.exists c |> ok_exn)

let opam_root =
  Fpath.of_string (OS.Env.req_var "OPAM_SWITCH_PREFIX" |> ok_exn) |> ok_exn

let etc = Fpath.(opam_root / "etc" / "odoc2docset")

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

let id_to_string_full id =
  let open Model.Paths_types.Identifier in
  let open Model.Names in
  let exception Not_printable in
  let rec i2s = function
    | `Root (_, x) -> UnitName.to_string x
    | `Page (_, x) -> PageName.to_string x
    | `CoreType x -> TypeName.to_string x
    | `CoreException x -> ExceptionName.to_string x
    | `Module (p, x) -> i2s (p :> any) ^ "." ^ ModuleName.to_string x
    | `ModuleType (p, x) -> i2s (p :> any) ^ "." ^ ModuleTypeName.to_string x
    | `Type (p, x) -> i2s (p :> any) ^ "." ^ TypeName.to_string x
    | `Extension (p, x) -> i2s (p :> any) ^ "." ^ ExtensionName.to_string x
    | `Exception (p, x) -> i2s (p :> any) ^ "." ^ ExceptionName.to_string x
    | `Value (p, x) -> i2s (p :> any) ^ "." ^ ValueName.to_string x
    | `Class (p, x) -> i2s (p :> any) ^ "." ^ ClassName.to_string x
    | `ClassType (p, x) -> i2s (p :> any) ^ "." ^ ClassTypeName.to_string x
    | `Method (p, x) -> i2s (p :> any) ^ "#" ^ MethodName.to_string x
    | `InstanceVariable (p, x) ->
        i2s (p :> any) ^ "#" ^ InstanceVariableName.to_string x
    | `Constructor (p, x) -> (
        match p with
        | `Type (p, _) -> i2s (p :> any) ^ "." ^ ConstructorName.to_string x
        | `CoreType _ -> ConstructorName.to_string x )
    | `Field (p, x) -> (
        match p with
        | `Root _ | `Module _ | `ModuleType _ | `Argument _ | `Type _ | `CoreType _
          ->
            i2s (p :> any) ^ "." ^ FieldName.to_string x
        | `Class _ | `ClassType _ -> i2s (p :> any) ^ "#" ^ FieldName.to_string x )
    | `Label _ | `Argument _ -> raise Not_printable
  in
  try Some (i2s id) with Not_printable -> None

let id_to_string id = Some (Model.Paths.Identifier.name id)

let id_kind (id : Model.Paths_types.Identifier.any) =
  match id with
  | `Type _ | `CoreType _ -> "Type"
  | `InstanceVariable _ -> "Variable"
  | `Module _ -> "Module"
  | `ModuleType _ -> "Interface"
  | `Method _ -> "Method"
  | `Field _ -> "Field"
  | `Label _ -> "Parameter"
  | `Exception _ | `CoreException _ -> "Exception"
  | `Class _ -> "Class"
  | `Page _ -> "Section"
  | `ClassType _ -> "Class"
  | `Value _ -> "Value"
  | `Argument _ -> "Parameter"
  | `Constructor _ -> "Constructor"
  | `Extension _ -> "Extension"
  | `Root _ -> "Package"

let is_hidden id =
  Option.map (id_to_string_full id) ~f:(String.is_substring ~substring:"__")
  |> Option.value ~default:true

(** Collect the identifiers (labeled with Dash types) in a compilation unit. *)
let ids_of_unit unit =
  let ids = ref [] in
  let visitor =
    object
      inherit Model.Maps.types

      inherit Model.Maps.identifier

      inherit Model.Maps.path

      inherit Model.Maps.fragment

      inherit Model.Maps.reference

      method! identifier x =
        ids := x :: !ids;
        x

      method! path x = x

      method! fragment x = x

      method! reference_any x = x

      method root x = x
    end
  in
  visitor#unit unit |> ignore;
  !ids

let url_to_string Html.Url.{ page; anchor; _ } =
  List.rev ("index.html" :: page) |> String.concat ~sep:"/" |> fun path ->
  if String.(anchor = "") then path else path ^ "#" ^ anchor

let update_index db docu_dir ids =
  let open Sqlite3 in
  let open Html in
  let stmt =
    prepare db "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?,?,?);"
  in
  exec db "BEGIN TRANSACTION;" |> Rc.ok_exn;
  let soups = Hashtbl.Poly.create () in
  (* Filter out the ids that don't have a string representation. *)
  List.filter_map ids ~f:(fun id ->
      match id_to_string_full id with
      | Some id_str -> Some (id, id_kind id, id_str)
      | None -> None)
  (* Filter out hidden ids. *)
  |> List.filter ~f:(fun (id, _, _) -> not (is_hidden id))
  (* Filter out the ids that don't have a URL. *)
  |> List.filter_map ~f:(fun (id, type_, id_str) ->
         let open Html in
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
             List.rev ("index.html" :: url.Url.page)
             |> String.concat ~sep:"/" |> Fpath.of_string |> ok_exn
           in
           Fpath.(docu_dir // html_path)
         in
         if OS.File.exists file |> ok_exn then Some (id, type_, id_str, url, file)
         else (
           Logs.warn (fun m ->
               m "Documentation file %a does not exist." Fpath.pp file);
           None ))
  (* Filter out the ids that have an anchor that does not exist in the
     documentation file. *)
  |> List.filter_map ~f:(fun (id, type_, id_str, url, file) ->
         let url =
           if String.(url.Url.anchor = "") then Some url
           else
             let soup =
               Hashtbl.find_or_add soups file ~default:(fun () ->
                   OS.File.read file |> ok_exn |> Soup.parse)
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
                 Logs.warn (fun m ->
                     m "Could not find anchor node for %s in %s." url.anchor
                       (Fpath.to_string file));
                 None
         in
         Option.map url ~f:(fun url -> (type_, id_str, url)))
  (* Insert ids into the database. *)
  |> List.iter ~f:(fun (type_, id_str, url) ->
         let url_str = url_to_string url in
         Logs.debug (fun m -> m "Inserting %s %s %s" id_str type_ url_str);
         let open Data in
         bind stmt 1 (TEXT id_str) |> Rc.ok_exn;
         bind stmt 2 (TEXT type_) |> Rc.ok_exn;
         bind stmt 3 (TEXT url_str) |> Rc.ok_exn;
         step stmt |> Rc.done_exn;
         reset stmt |> Rc.ok_exn);
  exec db "END TRANSACTION;" |> Rc.ok_exn;
  Hashtbl.iteri soups ~f:(fun ~key:file ~data:soup ->
      OS.File.write file (Soup.to_string soup) |> ok_exn)

let create_template output_path =
  let docset_dir = Fpath.of_string output_path |> ok_exn in
  let docset_name = Fpath.(rem_ext docset_dir |> basename) in
  let res_dir = Fpath.(docset_dir / "Contents" / "Resources") in
  let docu_dir = Fpath.(res_dir / "Documents") in
  let db_file = Fpath.(res_dir / "docSet.dsidx") in
  OS.Cmd.run Cmd.(v "rm" % "-rf" % p docset_dir) |> ok_exn;
  OS.Dir.create docu_dir |> ok_exn |> ignore;
  OS.File.write Fpath.(docset_dir / "Contents" / "Info.plist") (plist docset_name)
  |> ok_exn;
  OS.Cmd.(
    run_status ~quiet:true
      Cmd.(
        v "cp"
        % Fpath.(etc / "icon.png" |> to_string)
        % Fpath.(docset_dir / "icon.png" |> to_string)))
  |> ok_exn |> ignore;
  OS.Path.delete db_file |> ok_exn;
  (docset_dir, docu_dir, db_file)

let create_db db_file =
  let db = Sqlite3.db_open (Fpath.to_string db_file) in
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
  let%bind unit = Compilation_unit.load (Fs.File.of_string (Fpath.to_string path)) in
  let%bind () =
    match unit.Model.Lang.Compilation_unit.id with
    | `Root _ -> return ()
    | _ -> Error (`Msg (Fmt.str "Ignoring %a to avoid odoc segfault." Fpath.pp path))
  in
  let unit = Xref.Lookup.lookup unit in
  (* See comment in compile for explanation regarding the env duplication. *)
  let resolve_env = Env.build env (`Unit unit) in
  let%bind resolved = Odoc_xref.resolve (Env.resolver resolve_env) unit in
  let expand_env = Env.build env (`Unit resolved) in
  let%bind expanded = Odoc_xref.expand (Env.expander expand_env) resolved in
  (* Yes, again. *)
  Odoc_xref.Lookup.lookup expanded |> Odoc_xref.resolve (Env.resolver expand_env)

let rec odoc_files_exn d =
  OS.Dir.contents d |> ok_exn
  |> List.concat_map ~f:(fun f ->
         if OS.Dir.exists f |> ok_exn then odoc_files_exn f
         else if Fpath.has_ext "odoc" f then [ f ]
         else [])

let rec all_subdirs d =
  if OS.Dir.exists d |> ok_exn then
    d :: (OS.Dir.contents d |> ok_exn |> List.concat_map ~f:all_subdirs)
  else []

let populate_db include_dirs pkgs db docu_dir =
  List.iter include_dirs ~f:(fun d ->
      Logs.debug (fun m -> m "Include dir: %s" (Odoc.Fs.Directory.to_string d)));
  let builder = Odoc.Env.create ~important_digests:true ~directories:include_dirs in
  List.iter pkgs ~f:(fun pkg ->
      Logs.info (fun m -> m "Indexing %s." pkg);
      insert db pkg "Package" (sprintf "%s/index.html" pkg);
      let cachedir = Odig.cachedir pkg in
      odoc_files_exn cachedir
      |> List.iter ~f:(fun f ->
             Logs.debug (fun m -> m "Loading %s." (Fpath.to_string f));
             match load_unit builder f with
             | Ok unit ->
                 let ids = ids_of_unit unit in
                 update_index db docu_dir ids
             | Error (`Msg msg) -> Logs.err (fun m -> m "Resolve error: %s" msg)))

let tarix_to_sqlite tarix_fn sqlite_fn =
  (* Create new sqlite db. *)
  let db = Sqlite3.db_open (Fpath.to_string sqlite_fn) in
  Sqlite3.exec db
    "CREATE TABLE tarindex(path TEXT PRIMARY KEY COLLATE NOCASE, hash TEXT);\n\
     CREATE TABLE toextract(path TEXT PRIMARY KEY COLLATE NOCASE, hash TEXT);\n"
  |> Sqlite3.Rc.ok_exn;
  let insert_stmt =
    Sqlite3.prepare db "INSERT OR IGNORE INTO tarindex(path, hash) VALUES (?,?);"
  in
  Sqlite3.(exec db "BEGIN TRANSACTION;" |> Rc.ok_exn);
  In_channel.with_file (Fpath.to_string tarix_fn) ~f:(fun ch ->
      In_channel.fold_lines ch ~init:0 ~f:(fun lnum line ->
          ( if lnum > 0 then
            match String.split line ~on:' ' with
            | [ kind; off1; off2; len; fn ] ->
                if String.(kind = "0") then (
                  Sqlite3.(
                    bind insert_stmt 1 (TEXT fn) |> Rc.ok_exn;
                    bind insert_stmt 2
                      (TEXT (String.concat ~sep:" " [ off1; off2; len ]))
                    |> Rc.ok_exn;
                    step insert_stmt |> Rc.done_exn;
                    reset insert_stmt |> Rc.ok_exn) )
            | _ -> Logs.warn (fun m -> m "Unexpected line in tarix file: %s" line) );
          lnum + 1)
      |> ignore);
  Sqlite3.(exec db "END TRANSACTION;" |> Rc.ok_exn)

let compress_docset docset_dir =
  let temp_tgz = Caml.Filename.temp_file "tarix" ".tgz" in
  let temp_idx = Caml.Filename.temp_file "tarixIndex" ".dbtxt" in
  let temp_db = Caml.Filename.temp_file "docSet" ".dsidx" in
  let db_file = Fpath.(docset_dir / "Contents" / "Resources" / "docSet.dsidx") in
  (* Copy out the docset index. *)
  OS.Cmd.run Cmd.(v "cp" % p db_file % temp_db) |> ok_exn;
  (* Use tarix to compress the entire docset. *)
  OS.Env.set_var "TARIX" (Some (sprintf "-z -9 -f %s" temp_idx)) |> ok_exn;
  (* Convert the tarix index to a sqlite index. *)
  let docset_base, docset_rel = Fpath.split_base docset_dir in
  OS.Cmd.run
    Cmd.(
      tar % "-c" % "-f" % temp_tgz % "--use-compress-program" % "tarix" % "-C"
      % p docset_base % p docset_rel)
  |> ok_exn;
  (* Create a new docset template. *)
  OS.Cmd.run Cmd.(v "rm" % "-rf" % p docset_dir) |> ok_exn;
  let _ = create_template (Fpath.to_string docset_dir) in
  (* Generate a sqlite based tar index. *)
  tarix_to_sqlite
    (Fpath.of_string temp_idx |> ok_exn)
    Fpath.(docset_dir / "Contents" / "Resources" / "tarixIndex.db");
  (* Copy the docset index and compressed docset back in. *)
  OS.Cmd.run
    Cmd.(
      v "rm" % "-rf" % p Fpath.(docset_dir / "Contents" / "Resources" / "Documents"))
  |> ok_exn;
  OS.Cmd.run
    Cmd.(
      v "cp" % temp_tgz
      % p Fpath.(docset_dir / "Contents" / "Resources" / "tarix.tgz"))
  |> ok_exn;
  OS.Cmd.run
    Cmd.(
      v "cp" % temp_db
      % p Fpath.(docset_dir / "Contents" / "Resources" / "docSet.dsidx"))
  |> ok_exn

let run_quiet cmd = OS.Cmd.run_status ~quiet:true cmd |> ok_exn |> ignore

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
                false ))
        in
        if Logs.err_count () > 0 then Caml.exit 1 else names
  in
  let include_dirs =
    List.concat_map all_pkgs ~f:(fun pkg -> all_subdirs (Odig.cachedir pkg))
    |> List.map ~f:(fun d -> Odoc.Fs.Directory.of_string (Fpath.to_string d))
  in

  (* Create the docset template. *)
  let docset_dir, docu_dir, db_file = create_template output_path in

  (* Generate documentation using Odoc. *)
  Logs.info (fun m -> m "Running odoc.");
  OS.Cmd.(Cmd.(v "odig" % "odoc" %% of_list pkgs) |> run_status ~quiet:true)
  |> ok_exn |> ignore;
  Logs.info (fun m -> m "Done running odoc.");

  (* Copy documentation. *)
  Logs.info (fun m -> m "Copying documentation.");
  List.iter pkgs ~f:(fun pkg ->
      Logs.debug (fun m -> m "Copying %s." pkg);
      let doc_dir = Odig.htmldir pkg in
      let cmd = Cmd.(v "cp" % "-r" % p doc_dir % p docu_dir) in
      OS.Cmd.run_status ~quiet:true cmd |> ok_exn |> ignore);

  (* Copy theme CSS & JS. *)
  let theme_path = Fpath.(Odig.theme_prefix / theme) in
  if not (OS.Dir.exists theme_path |> ok_exn) then
    Logs.err (fun m -> m "Theme %s does not exist." theme)
  else
    run_quiet Cmd.(v "cp" % "-r" % p theme_path % p Fpath.(docu_dir / "_odoc-theme"));

  run_quiet
    Cmd.(
      v "cp"
      % p
          Fpath.(
            opam_prefix / "share" / "odoc" / "odoc-theme" / "default"
            / "highlight.pack.js")
      % p docu_dir);

  Logs.info (fun m -> m "Done copying documentation.");

  Logs.info (fun m -> m "Creating index.");
  let db = create_db db_file in
  populate_db include_dirs pkgs db docu_dir;
  Logs.info (fun m -> m "Done creating index.");
  if compress then (
    Logs.info (fun m -> m "Compressing docset.");
    compress_docset docset_dir;
    Logs.info (fun m -> m "Done compressing docset.") )

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
    Arg.(value & opt string "default" & info [ "t"; "theme" ] ~doc)
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
