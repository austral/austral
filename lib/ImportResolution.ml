open Identifier
open Common
open ModuleSystem
open Imports
open Cst
open BuiltIn
open Error
open Env

(** Represents an Austral import declaration, that is, in some code like:

    {import Foo.Bar (A as D, B, C);}

    The first symbol would correspond to this {ImportDecl}:

    {ImportDecl ("Foo.Bar", "A", "D")}
 *)
type import_decl = ImportDecl of module_name * identifier * identifier

(** Return the nickname if non-{None}, otherwise return the name. *)
let nickname_or_real_name (name: identifier) (nickname: identifier option): identifier =
  match nickname with
  | (Some nick) -> nick
  | None -> name

(** Take a list of import lists, and flatten it to a list of import decls *)
let flatten_imports (list: concrete_import_list list): import_decl list =
  let ll = List.map
             (fun (ConcreteImportList (mn, imports)) ->
               List.map (fun (ConcreteImport (name, nickname)) ->
                   ImportDecl (mn, name, nickname_or_real_name name nickname))
                 imports)
             list in
  List.concat ll

let resolve_import (env: env) (kind: module_kind) (imports: import_map) (idecl: import_decl): import_map =
  let (ImportDecl (module_name, name, nickname)) = idecl in
  let sname = make_sident module_name name in
  match get_decl_by_name env sname with
  | Some decl ->
     if is_importable decl then
       match get_symbol imports nickname with
       | Some _ ->
          err "Colliding imports"
       | None ->
          if (equal_module_name module_name memory_module_name) && (kind = SafeModule) then
            err "Cannot import from the Austral.Memory module in a safe module."
          else
            add_symbol imports (make_qident (module_name, name, nickname))
     else
       err "Declaration is not importable"
  | None ->
     err ("No declaration with the name '"
          ^ (ident_string name)
          ^ "' in the module '"
          ^ (mod_name_string module_name)
          ^ "'")

let rec resolve' (env: env) (kind: module_kind) (imports: import_map) (list: import_decl list): import_map =
  match list with
  | (first::rest) ->
     resolve' env kind (resolve_import env kind imports first) rest
  | [] ->
     imports

let rec add_instances (im: import_map) (is: decl_id list): import_map =
  match is with
  | first::rest ->
     add_instance (add_instances im rest) first
  | [] ->
     im

let module_names (cil: concrete_import_list list): module_name list =
  List.map (fun (ConcreteImportList (mn, _)) -> mn) cil

let module_defined_instances (env: env) (mn: module_name): decl_id list =
  match (get_module_by_name env mn) with
  | Some mod_rec ->
     let { id; _ } = mod_rec in
     module_instances env id
  | None ->
     []

let import_instances (env: env) (list: concrete_import_list list): decl_id list =
  let mns = module_names list in
  let instances = List.map (module_defined_instances env) mns in
  List.flatten instances

let resolve (importing_module: module_name) (kind: module_kind) (env: env) (list: concrete_import_list list): import_map =
  let im: import_map = resolve' env kind (empty_map importing_module) (flatten_imports list) in
  let ins: decl_id list = import_instances env list in
  add_instances im ins
