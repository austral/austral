(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Imports
open Cst
open BuiltIn
open Error
open Id
open Env
open EnvTypes
open EnvUtils

module Errors = struct
  let double_import name =
    austral_raise DeclarationError [
      Text "Colliding imports: a declaration with name ";
      Code (ident_string name);
      Text " has already been imported.";
      Break;
      Text "Consider giving it a different nickname using ";
      Code "as";
      Text "."
    ]

  let import_memory_in_safe_module () =
    austral_raise DeclarationError [
      Text "Cannot import from the ";
      Code "Austral.Memory";
      Text " module in a safe module.";
      Break;
      Text "Consider using a safe wrapper or adding ";
      Code "pragma Unsafe_Module;"
    ]

  let privacy_error name =
    austral_raise DeclarationError [
      Code (ident_string name);
      Text " is private and cannot be imported."
    ]

  let unresolved ~name ~module_name =
    austral_raise DeclarationError [
      Text "No declaration with the name ";
      Code (ident_string name);
      Text " in the module ";
      Code (mod_name_string module_name)
    ]
end

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
       | Some _ -> Errors.double_import name
       | None ->
          if (equal_module_name module_name memory_module_name) && (kind = SafeModule) then
            Errors.import_memory_in_safe_module ()
          else
            add_symbol imports (make_qident (module_name, name, nickname))
     else
       Errors.privacy_error name
  | None ->
     Errors.unresolved ~name ~module_name

let rec resolve' (env: env) (kind: module_kind) (imports: import_map) (list: import_decl list): import_map =
  match list with
  | (first::rest) ->
     resolve' env kind (resolve_import env kind imports first) rest
  | [] ->
     imports

let rec add_instances (im: import_map) (is: decl_id list): import_map =
  match is with
  | first::rest ->
     add_instance_to_imports (add_instances im rest) first
  | [] ->
     im

let module_names (cil: concrete_import_list list): module_name list =
  List.map (fun (ConcreteImportList (mn, _)) -> mn) cil

let module_defined_instances (env: env) (mn: module_name): decl_id list =
  match (get_module_by_name env mn) with
  | Some mod_rec ->
     let ModRec { id; _ } = mod_rec in
     let decls: decl list = module_public_instances env id in
     List.map decl_id decls
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
