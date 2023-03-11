(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** The linked representation is essentially the same as the combined
    representation, but declarations are linked to their corresponding entry in
    the environment. *)
open Identifier
open Common
open Imports
open Ast
open Type
open TypeParameter
open TypeParameters
open Id

type linked_module = LinkedModule of {
      mod_id: mod_id;
      name: module_name;
      kind: module_kind;
      interface_docstring: docstring;
      interface_imports: import_map;
      body_docstring: docstring;
      body_imports: import_map;
      decls: linked_definition list;
    }

and linked_definition =
  | LConstant of decl_id * vis * identifier * ty * aexpr * docstring
  | LRecord of decl_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
  | LUnion of decl_id * type_vis * identifier * typarams * universe * linked_case list * docstring
  | LFunction of decl_id * vis * identifier * typarams * value_parameter list * ty * astmt * docstring * pragma list
  | LTypeclass of decl_id * vis * identifier * type_parameter * linked_method_decl list * docstring
  | LInstance of decl_id * vis * qident * typarams * ty * linked_method_def list * docstring

and linked_case = LCase of decl_id * identifier * typed_slot list

and linked_method_decl = LMethodDecl of decl_id * identifier * value_parameter list * ty * docstring

and linked_method_def = LMethodDef of ins_meth_id * identifier * value_parameter list * ty * docstring * astmt
