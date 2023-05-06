(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Imports
open Ast
open Type
open TypeParameter
open TypeParameters
open Span

type combined_module = CombinedModule of {
      name: module_name;
      kind: module_kind;
      interface_docstring: docstring;
      interface_imports: import_map;
      body_docstring: docstring;
      body_imports: import_map;
      decls: combined_definition list;
    }

and combined_definition =
  | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
  | CRecord of span * type_vis * identifier * typarams * universe * qslot list * docstring
  | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
  | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
  | CTypeclass of span * vis * identifier * type_parameter * combined_method_decl list * docstring
  | CInstance of span * vis * qident * typarams * qtypespec * combined_method_def list * docstring

and qslot = QualifiedSlot of identifier * qtypespec

and qcase = QualifiedCase of identifier * qslot list

and qparam = QualifiedParameter of identifier * qtypespec

and combined_method_decl = CMethodDecl of identifier * typarams * qparam list * qtypespec * docstring

and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
