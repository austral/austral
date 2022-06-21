open Identifier
open Type
open TypeParser
(*open Error*)

module BindingsMap =
  Map.Make(
      struct
        open Identifier
        type t = (identifier * qident)
        let compare (n, f) (n', f') =
          (* Turn a qident into a string. *)
          let qs (qname: qident): string =
            (mod_name_string (source_module_name qname))
            ^ (ident_string (original_name qname))
            ^ (ident_string (local_name qname))
          in
          let a = (ident_string n) ^ (qs f)
          and b = (ident_string n') ^ (qs f')
          in
          compare a b
      end
    )


type type_bindings = TypeBindings of ty BindingsMap.t

let binding_count (TypeBindings m) =
  BindingsMap.cardinal m

let bindings_list (TypeBindings m) =
  List.map (fun ((n, f), t) -> (n, f, t)) (BindingsMap.bindings m)

let empty_bindings = TypeBindings BindingsMap.empty

let show_bindings (TypeBindings m) =
  let show_binding ((n, f), t) =
    (show_identifier n) ^ " from " ^ (qident_debug_name f) ^ " => " ^ (show_ty t)
  in
  "TypeBindings {" ^ (String.concat ", " (List.map show_binding (BindingsMap.bindings m))) ^ "}"

  (*
let binding_conflict name from ty ty' =
  let str = "Conflicting type variables: the variable "
            ^ ident_string name
            ^ " (from "
            ^ (qident_debug_name from)
            ^ ") has values "
            ^ type_string ty
            ^ " and "
            ^ type_string ty'
            ^ "."
  in
  err str
   *)
let get_binding (TypeBindings m) name from =
  BindingsMap.find_opt (name, from) m

(* Add a binding to the map.

   If a binding with this name already exists, fail if the types are
   distinct. *)
let add_binding (TypeBindings m) name from ty =
  match BindingsMap.find_opt (name, from) m with
  | Some ty' ->
     if equal_ty ty ty' then
       TypeBindings m
     else
       (* FIXME: Should we fail here? *)
       (* let _ = print_endline (show_bindings (TypeBindings m)) in
       binding_conflict name from ty ty' *)
       (* Power through it. *)
       TypeBindings (BindingsMap.add (name, from) ty' m)
  | None ->
     TypeBindings (BindingsMap.add (name, from) ty m)

(* Add multiple bindings to a bindings map. *)
let rec add_bindings bs triples =
  match triples with
  | (name, from, ty)::rest -> add_bindings (add_binding bs name from ty) rest
  | [] -> bs

let merge_bindings (TypeBindings a) (TypeBindings b) =
  let m = add_bindings empty_bindings (List.map (fun ((n, f), t) -> (n, f, t)) (BindingsMap.bindings a)) in
  add_bindings m (List.map (fun ((n, f), t) -> (n, f, t)) (BindingsMap.bindings b))

let rec replace_variables bindings ty =
  match ty with
  | Unit ->
     Unit
  | Boolean ->
     Boolean
  | Integer (s, w) ->
     Integer (s, w)
  | SingleFloat ->
     SingleFloat
  | DoubleFloat ->
     DoubleFloat
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
  | TyVar (TypeVariable (n, u, from, constraints)) ->
     (match get_binding bindings n from with
      | Some ty -> ty
      | None -> TyVar (TypeVariable (n, u, from, constraints)))
  | StaticArray ty ->
     StaticArray (replace_variables bindings ty)
  | RegionTy r ->
     RegionTy r
  | ReadRef (ty, region) ->
     ReadRef (replace_variables bindings ty, replace_variables bindings region)
  | WriteRef (ty, region) ->
     WriteRef (replace_variables bindings ty, replace_variables bindings region)
  | Address ty ->
     Address (replace_variables bindings ty)
  | Pointer ty ->
     Pointer (replace_variables bindings ty)
  | MonoTy id ->
     MonoTy id

let rec bindings_from_list lst =
  match lst with
  | (name, source, ty)::rest ->
     let bindings = empty_bindings in
     let bindings = add_binding bindings name source ty in
     merge_bindings bindings (bindings_from_list rest)
  | [] ->
   empty_bindings

let pp_type_bindings _ _ = ()

let show_type_bindings = show_bindings
