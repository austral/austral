open Identifier
open IdentifierMap
open Type
open TypeParser
open Error

type type_bindings = TypeBindings of ty IdentifierMap.t

let binding_count (TypeBindings m) =
  IdentifierMap.cardinal m

let empty_bindings = TypeBindings IdentifierMap.empty

let binding_conflict name ty ty' =
  let str = "Conflicting type variables: the variable "
            ^ ident_string name
            ^ " has values "
            ^ type_string ty
            ^ " and "
            ^ type_string ty'
            ^ "."
  in
  err str

let get_binding (TypeBindings m) name =
  IdentifierMap.find_opt name m

(* Add a binding to the map.

   If a binding with this name already exists, fail if the types are
   distinct. *)
let add_binding (TypeBindings m) name ty =
  match IdentifierMap.find_opt name m with
      | (Some ty') -> if ty = ty' then
                        TypeBindings m
                      else
                        binding_conflict name ty ty'
      | None -> TypeBindings (IdentifierMap.add name ty m)

(* Add multiple bindings to a bindings map. *)
let rec add_bindings bs pairs =
  match pairs with
  | (name, ty)::rest -> add_bindings (add_binding bs name ty) rest
  | [] -> bs

let merge_bindings (TypeBindings a) (TypeBindings b) =
  let m = add_bindings empty_bindings (IdentifierMap.bindings a) in
  add_bindings m (IdentifierMap.bindings b)

let rec replace_variables bindings ty =
  match ty with
  | TyVar (TypeVariable (n, u)) ->
     (match get_binding bindings n with
      | Some ty -> ty
      | None -> TyVar (TypeVariable (n, u)))
  | NamedType (n, a, u) ->
     let a' = List.map (replace_variables bindings) a in
     if u = TypeUniverse then
       let u' = if any_arg_is_linear a' then
                  LinearUniverse
                else
                  if any_arg_is_type a' then
                    TypeUniverse
                  else
                    FreeUniverse
       in
       NamedType (n, a', u')
     else
       NamedType (n, a', u)
  | t ->
     t

let show_bindings (TypeBindings m) =
  let show_binding (n, t) =
    (show_identifier n) ^ " => " ^ (show_ty t)
  in
  "TypeBindings {" ^ (String.concat ", " (List.map show_binding (IdentifierMap.bindings m))) ^ ")"
