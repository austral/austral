open Identifier
open Env
open Type
open TypeStripping
open MonoType2
open Error

let rec monomorphize_ty (env: env) (ty: stripped_ty): (mono_ty * env) =
  match ty with
  | SUnit ->
    (MonoUnit, env)
  | SBoolean ->
    (MonoBoolean, env)
  | SInteger (s, w) ->
    (MonoInteger (s, w), env)
  | SSingleFloat ->
    (MonoSingleFloat, env)
  | SDoubleFloat ->
    (MonoDoubleFloat, env)
  | SArray elem_ty ->
    let (elem_ty, env) = monomorphize_ty env elem_ty in
    (MonoArray elem_ty, env)
  | SReadRef ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoReadRef ty, env)
  | SWriteRef ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoWriteRef ty, env)
  | SRawPointer ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoRawPointer ty, env)
  | SNamedType (name, args) ->
    let (args, env) = monomorphize_ty_list env args in
    (match get_decl_by_name env (qident_to_sident name) with
     | Some decl ->
       (match decl with
        | TypeAlias { id; _} ->
          let (env, mono_id) = add_or_get_type_alias_monomorph env id args in
          (MonoNamedType mono_id, env)
        | Record { id; _ } ->
          let (env, mono_id) = add_or_get_record_monomorph env id args in
          (MonoNamedType mono_id, env)
        | Union { id; _ } ->
          let (env, mono_id) = add_or_get_union_monomorph env id args in
          (MonoNamedType mono_id, env)
        | _ ->
          err "internal: named type points to something that isn't a type")
     | None ->
       err "internal")

and monomorphize_ty_list (env: env) (tys: stripped_ty list): (mono_ty list * env) =
  match tys with
  | first::rest ->
     let (first, env) = monomorphize_ty env first in
     let (rest, env) = monomorphize_ty_list env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

let strip_and_mono (env: env) (ty: ty): (mono_ty * env) =
  let ty = strip_type ty in
  monomorphize_ty env ty
