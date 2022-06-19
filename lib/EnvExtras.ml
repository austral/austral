open Id
open Identifier
open Env
open EnvTypes
open EnvUtils
open Error

let get_decl_by_name_or_die (env: env) (name: sident): decl =
  match get_decl_by_name env name with
  | Some decl -> decl
  | None ->
     internal_err "No declaration with name."

let get_decl_name_or_die (env: env) (id: decl_id): string =
  match get_decl_by_id env id with
  | Some decl ->
     (match (decl_name decl) with
      | Some name ->
         (ident_string name)
      | None ->
         err "decl has no name")
  | None ->
     err "internal"
