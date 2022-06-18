open Identifier
open Common
open Id
open EnvTypes
open Type
open Error

let decl_id (decl: decl): decl_id =
  match decl with
  | Constant { id; _ } -> id
  | TypeAlias { id; _ } -> id
  | Record { id; _ } -> id
  | Union { id; _ } -> id
  | UnionCase { id; _ } -> id
  | Function { id; _ } -> id
  | TypeClass { id; _ } -> id
  | TypeClassMethod { id; _ } -> id
  | Instance { id; _ } -> id

let decl_mod_id (decl: decl): mod_id =
  match decl with
  | Constant { mod_id; _ } -> mod_id
  | TypeAlias { mod_id; _ } -> mod_id
  | Record { mod_id; _ } -> mod_id
  | Union { mod_id; _ } -> mod_id
  | UnionCase { mod_id; _ } -> mod_id
  | Function { mod_id; _ } -> mod_id
  | TypeClass { mod_id; _ } -> mod_id
  | TypeClassMethod { mod_id; _ } -> mod_id
  | Instance { mod_id; _ } -> mod_id

let decl_name (decl: decl): identifier option =
  match decl with
  | Constant { name; _ } -> Some name
  | TypeAlias { name; _ } -> Some name
  | Record { name; _ } -> Some name
  | Union { name; _ } -> Some name
  | UnionCase { name; _ } -> Some name
  | Function { name; _ } -> Some name
  | TypeClass { name; _ } -> Some name
  | TypeClassMethod { name; _ } -> Some name
  | Instance _ -> None

let is_importable (decl: decl): bool =
  let importable_vis = function
    | VisPublic -> true
    | VisPrivate -> false
  and importable_type = function
    | TypeVisPublic -> true
    | TypeVisOpaque -> true
    | TypeVisPrivate -> false
  in
  match decl with
  | Constant { vis; _ } -> importable_vis vis
  | TypeAlias { vis; _ } -> importable_type vis
  | Record { vis; _ } -> importable_type vis
  | Union { vis; _ } -> importable_type vis
  | UnionCase { vis; _ } -> importable_vis vis
  | Function { vis; _ } -> importable_vis vis
  | TypeClass { vis; _ } -> importable_vis vis
  | TypeClassMethod { vis; _ } -> importable_vis vis
  | Instance _ -> true

let union_case_to_typed_case (decl: decl): typed_case =
  match decl with
  | UnionCase { name; slots; _ } ->
     TypedCase (name, slots)
  | _ ->
     internal_err "Declaration is not a union case"
