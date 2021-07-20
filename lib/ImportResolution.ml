open Identifier
open Common
open ModuleSystem
open Imports
open Cst
open Semantic
open BuiltIn
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

let resolve_import (menv: menv) (kind: module_kind) (imports: import_map) (idecl: import_decl): import_map =
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
               if (equal_module_name module_name memory_module_name) && (kind = SafeModule) then
                 err "Cannot import from the Austral.Memory module in a safe module."
               else
                 add_symbol imports (make_qident (module_name, name, nickname)))
         else
           err "Declaration is not importable"
      | None ->
         if has_union_constructor_with_name smodule name then
           add_symbol imports (make_qident (module_name, name, nickname))
         else
           err "No declaration with this name")
  | None ->
     err ("No module with this name: " ^ (mod_name_string module_name))

let rec resolve' (menv: menv) (kind: module_kind) (imports: import_map) (list: import_decl list): import_map =
  match list with
  | (first::rest) ->
     resolve' menv kind (resolve_import menv kind imports first) rest
  | [] ->
     imports

let resolve (importing_module: module_name) (kind: module_kind) (menv: menv) (list: concrete_import_list list): import_map =
  resolve' menv kind (empty_map importing_module) (flatten_imports list)
