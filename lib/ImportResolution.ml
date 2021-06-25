open Identifier
open ModuleSystem
open Imports
open Cst
open Semantic
open Error

type import_decl = ImportDecl of module_name * identifier * identifier

let nickname_or_real_name (name: identifier) (nickname: identifier option): identifier =
  match nickname with
  | (Some nick) -> nick
  | None -> name

(* Take a list of import lists, and flatten it to a list of import decls *)
let flatten_imports (list: concrete_import_list list): import_decl list =
  let ll = List.map
             (fun (ConcreteImportList (mn, imports)) ->
               List.map (fun (ConcreteImport (name, nickname)) ->
                   ImportDecl (mn, name, nickname_or_real_name name nickname))
                 imports)
             list in
  List.concat ll

let resolve_import (menv: menv) (imports: import_map) (idecl: import_decl): import_map =
  let (ImportDecl (module_name, name, nickname)) = idecl in
  match get_module menv module_name with
  | (Some smodule) ->
     (match get_declaration (smodule, name) with
      | (Some decl) ->
         if is_importable decl then
           (match get_symbol imports nickname with
            | (Some _) ->
               err "Colliding imports"
            | None ->
               add_symbol imports (make_qident (module_name, name, nickname)))
         else
           err "Declaration is not importable"
      | None ->
         err "No declaration with this name")
  | None ->
     err "No module with this name"

let rec resolve' (menv: menv) (imports: import_map) (list: import_decl list): import_map =
  match list with
  | (first::rest) ->
     resolve' menv (resolve_import menv imports first) rest
  | [] ->
     imports

let resolve (importing_module: module_name) (menv: menv) (list: concrete_import_list list): import_map =
  resolve' menv (empty_map importing_module) (flatten_imports list)
