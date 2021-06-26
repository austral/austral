open Identifier
open IdentifierMap
open Error
open Type


type type_bindings = TypeBindings of ty IdentifierMap.t

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
     NamedType (n, List.map (replace_variables bindings) a, u)
  | t ->
     t
