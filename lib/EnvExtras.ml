open Id
open Identifier
open Env
open EnvTypes
open EnvUtils
open Error

let get_module_or_die (env: env) (id: mod_id): mod_rec =
  match get_module_by_id env id with
  | Some r -> r
  | None -> internal_err "No module with ID."

let get_module_name_or_die (env: env) (id: mod_id): module_name =
  let (ModRec { name; _ }) = get_module_or_die env id in
  name

let get_decl_by_name_or_die (env: env) (name: sident): decl =
  match get_decl_by_name env name with
  | Some decl -> decl
  | None ->
     internal_err ("No declaration with name `"
                   ^ ident_string (sident_name name)
                   ^ "`.")

let get_decl_sident_or_die (env: env) (id: decl_id): sident =
  match get_decl_by_id env id with
  | Some decl ->
     (match (decl_name decl) with
      | Some name ->
         make_sident (get_module_name_or_die env (decl_mod_id decl)) name
      | None ->
         err "decl has no name")
  | None ->
   internal_err ("decl with ID `" 
                  ^ (show_decl_id id)
                  ^ "` doesn't exist")

let get_decl_name_or_die (env: env) (id: decl_id): string =
  match get_decl_by_id env id with
  | Some decl ->
     (match (decl_name decl) with
      | Some name ->
         (ident_string name)
      | None ->
         err "decl has no name")
  | None ->
   internal_err ("decl with ID `" 
                  ^ (show_decl_id id)
                  ^ "` doesn't exist")

let get_method_id_or_die (env: env) (typeclass_id: decl_id) (name: qident) =
  match get_method_from_typeclass_id_and_name env typeclass_id (original_name name) with
  | Some (TypeClassMethod { id; _ }) ->
     id
  | _ ->
     internal_err ("Could not retrieve method from the typeclass ID `"
                   ^ (show_decl_id typeclass_id)
                   ^ "` and the name `"
                   ^ (qident_debug_name name)
                   ^ "`.")
