open Sexplib
open AcmFile
open Identifier
open Id
open ModIdSet
open Env
open Type
open Error

type sexp = Sexp.t

let fail (msg: string) =
  err ("ACM file parsing failed: " ^ msg)

(* mod_id *)

let ser_mod_id (env: env) (id: mod_id): sexp =
  match get_module_by_id env id with
  | Some (ModRec { name; _ }) ->
     sexp_of_module_name name
  | None ->
     err "internal"

(* ModIdSet.t *)

let ser_mod_id_set (env: env) (set: ModIdSet.t): sexp =
  List (List.map (ser_mod_id env) (List.of_seq (ModIdSet.to_seq set)))

(* compiled_decl *)

let ser_compiled_decl (env: env) (decl: compiled_decl): sexp =
  let _ = env in
  match decl with
  | CompiledConstant { name; ty; } ->
     List [Atom "CompiledConstant"; List [sexp_of_identifier name; sexp_of_ty ty]]
  | _ ->
     err "Not implemented yet"

(* compiled_module *)

let ser_compiled_module (env: env) (cm: compiled_module): sexp =
  let CompiledModule { name; imports_from; decls; } = cm in
  List [
      Atom "CompiledModule";
      ser_module_name name;
      ser_mod_id_set env imports_from;
      List (List.map (ser_compiled_decl env) decls)
    ]
