(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open IdentifierMap
open ModuleNameSet
open Id
open Error

type import_map =
  ImportMap of {
      import_into: module_name;
      symbols: qident IdentifierMap.t;
      instances: decl_id list;
    }

let empty_map (n: module_name): import_map =
  ImportMap {
      import_into = n;
      symbols = IdentifierMap.empty;
      instances = [];
    }

let get_symbol (im: import_map) (name: identifier): qident option =
  let (ImportMap { symbols; _ }) = im in
  IdentifierMap.find_opt name symbols

let add_symbol (im: import_map) (q: qident) =
  match get_symbol im (local_name q) with
  | Some _ ->
     err "Cannot add symbol to the import map twice."
  | None ->
     let (ImportMap { import_into; symbols; instances }) = im in
     ImportMap {
         import_into = import_into;
         symbols = IdentifierMap.add (local_name q) q symbols;
         instances = instances
       }

let add_instance_to_imports (im: import_map) (id: decl_id): import_map =
  let (ImportMap { import_into; symbols; instances }) = im in
  ImportMap {
      import_into = import_into;
      symbols = symbols;
      instances = id :: instances
    }

let importing_module (im: import_map): module_name =
  let (ImportMap { import_into; _ }) = im in
  import_into

let imported_instances (im: import_map): decl_id list =
  let (ImportMap { instances; _ }) = im in
  instances

let modules_imported_from (im: import_map): ModuleNameSet.t =
  let ImportMap { symbols; _ } = im in
  let names: qident list = List.map (fun (_, q) -> q) (List.of_seq (IdentifierMap.to_seq symbols)) in
  let modnames: module_name list = List.map source_module_name names in
  ModuleNameSet.of_list modnames
