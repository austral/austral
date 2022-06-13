open Sexplib
open AcmFile
open Identifier
open Id
open ModIdSet
open Env
open Error

type sexp = Sexp.t

let fail (msg: string) =
  err ("ACM file parsing failed: " ^ msg)

(* acm_type_vis *)

let ser_acm_type_vis (vis: acm_type_vis): sexp =
  match vis with
  | AcmTypeVisPublic -> Atom "ACM_TYPE_VIS_PUBLIC"
  | AcmTypeVisOpaque -> Atom "ACM_TYPE_VIS_OPAQUE"

let par_acm_type_vis (sexp: sexp): acm_type_vis =
  match sexp with
  | Atom "ACM_TYPE_VIS_PUBLIC" -> AcmTypeVisPublic
  | Atom "ACM_TYPE_VIS_OPAQUE" -> AcmTypeVisOpaque
  | _ -> fail "bad acm_type_vis"

(* module_name *)

let ser_module_name (mn: module_name): sexp =
  Atom (mod_name_string mn)

let par_module_name (sexp: sexp): module_name =
  match sexp with
  | Atom s -> make_mod_name s
  | _ -> fail "bad module_name"

(* mod_id *)

let ser_mod_id (env: env) (id: mod_id): sexp =
  match get_module_by_id env id with
  | Some (ModRec { name; _ }) ->
     ser_module_name name
  | None ->
     err "internal"

(* ModIdSet.t *)

let ser_mod_id_set (env: env) (set: ModIdSet.t): sexp =
  List (List.map (ser_mod_id env) (List.of_seq (ModIdSet.to_seq set)))

(* compiled_decl *)

let ser_compiled_decl (env: env) (decl: compiled_decl): sexp =
  let _ = (env, decl) in
  Atom ""

(* compiled_module *)

let ser_compiled_module (env: env) (cm: compiled_module): sexp =
  let CompiledModule { name; imports_from; decls; } = cm in
  List [
      Atom "CompiledModule";
      ser_module_name name;
      ser_mod_id_set env imports_from;
      List (List.map (ser_compiled_decl env) decls)
    ]
