open Core
open Odoc_model
open Lang

let any x = (x :> Paths.Identifier.t)

let rec compilation_unit (x : Compilation_unit.t) =
  match x.content with
  | Pack _ ->
      Logs.warn (fun m -> m "Ignoring module pack");
      []
  | Module s -> signature s

and signature (x : Signature.t) =
  List.concat_map x.items ~f:(function
    | Module (_, x) -> module_ x
    | ModuleType x -> module_type x
    | Type (_, x) -> type_decl x
    | Exception x -> exception_ x
    | Value x -> value x
    | External x -> external_ x
    | Class (_, x) -> class_ x
    | ClassType (_, x) -> class_type x
    | Include x -> include_ x
    | ModuleSubstitution x ->
        Logs.warn (fun m ->
            m "Ignoring module substitution: %s" @@ Paths.Identifier.name x.id);
        []
    | TypeSubstitution _ ->
        Logs.warn (fun m -> m "Ignoring type substitution");
        []
    | TypExt _ ->
        Logs.warn (fun m -> m "Ignoring type extension");
        []
    | Open _ | Comment _ -> [])

and include_ (x : Include.t) = signature x.expansion.content

and module_ (x : Module.t) =
  let ids =
    match x.type_ with
    | ModuleType x -> module_type_expr x
    | Alias (_, Some exp) -> simple_expansion exp
    | Alias (_, None) -> []
  in
  any x.id :: ids

and simple_expansion = function
  | ModuleType.Signature x -> signature x
  | Functor (_, x) -> simple_expansion x

and module_type (x : ModuleType.t) =
  let ids = match x.expr with Some (Signature s) -> signature s | _ -> [] in
  any x.id :: ids

and module_type_expr = function
  | Signature x -> signature x
  | Functor (_, x) -> module_type_expr x
  | _ -> []

and type_decl (x : TypeDecl.t) =
  let ids =
    match x.representation with
    | Some (Variant cs) ->
        List.map cs ~f:(fun (c : TypeDecl.Constructor.t) -> any c.id)
    | Some (Record fs) -> List.map fs ~f:(fun (f : TypeDecl.Field.t) -> any f.id)
    | Some Extensible | None -> []
  in
  any x.id :: ids

and exception_ (x : Exception.t) = [ any x.id ]

and value (x : Value.t) = [ any x.id ]

and external_ (x : External.t) = [ any x.id ]

and class_ (x : Class.t) =
  let ids = match x.expansion with Some cs -> class_signature cs | None -> [] in
  any x.id :: ids

and class_type (x : ClassType.t) =
  let ids = match x.expansion with Some cs -> class_signature cs | None -> [] in
  any x.id :: ids

and class_signature (x : ClassSignature.t) =
  List.filter_map x.items ~f:(function
    | Method x -> Some (any x.id)
    | InstanceVariable x -> Some (any x.id)
    | _ -> None)
