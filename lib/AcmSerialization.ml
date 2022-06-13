open Sexplib
open AcmFile
open Identifier
open ModuleNameSet
open Error

type sexp = Sexp.t

(* ModIdSet.t *)

let ser_mod_id_set (set: ModuleNameSet.t): sexp =
  List (List.map sexp_of_module_name (List.of_seq (ModuleNameSet.to_seq set)))

let par_mod_id_set (sexp: sexp): ModuleNameSet.t =
  match sexp with
  | List atoms ->
     ModuleNameSet.of_list (List.map module_name_of_sexp atoms)
  | _ ->
     err "internal: bad parse"

(* compiled_module *)

let ser_compiled_module (cm: compiled_module): sexp =
  let CompiledModule { name; imports_from; decls; } = cm in
  List [
      Atom "CompiledModule";
      sexp_of_module_name name;
      ser_mod_id_set imports_from;
      List (List.map sexp_of_compiled_decl decls)
    ]

let par_compiled_module (sexp: sexp): compiled_module =
  match sexp with
  | List [Atom "CompiledModule"; name; imports; decls] ->
     CompiledModule {
         name = module_name_of_sexp name;
         imports_from = par_mod_id_set imports;
         decls = (match decls with
                  | List decls ->
                     List.map compiled_decl_of_sexp decls
                  | _ ->
                    err "internal: bad parse");
       }
  | _ ->
     err "internal: bad acm parse"
