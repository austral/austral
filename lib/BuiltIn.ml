(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Cst

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name BuiltInNames.pervasive_module

let pervasive_imports =
  ConcreteImportList (
      pervasive_module_name,
      List.map (fun name -> ConcreteImport (make_ident name, None)) BuiltInNames.pervasive_import_names
    )

(* Austral.Memory *)

let memory_module_name = make_mod_name BuiltInNames.memory_module

let is_address_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident BuiltInNames.address_name))

let is_pointer_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident BuiltInNames.pointer_name))
