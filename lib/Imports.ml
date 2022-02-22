open Identifier
open IdentifierMap
open Env

type import_map =
  ImportMap of {
      import_into: module_name;
      symbols: qident IdentifierMap.t;
      instances: semantic_instance list;
    }

let empty_map (n: module_name): import_map =
  ImportMap {
      import_into = n;
      symbols = IdentifierMap.empty;
      instances = [];
    }

let add_symbol (im: import_map) (q: qident) =
  let (ImportMap { import_into; symbols; instances }) = im in
  ImportMap {
      import_into = import_into;
      symbols = IdentifierMap.add (local_name q) q symbols;
      instances = instances
    }

let add_instance (im: import_map) (id: decl_id): import_map =
  let (ImportMap { import_into; symbols; instances }) = im in
  ImportMap {
      import_into = import_into;
      symbols = symbols;
      classes = classes;
      instances = id :: instances
    }

let importing_module (im: import_map): module_name =
  let (ImportMap { import_into; _ }) = im in
  import_into

let get_symbol (im: import_map) (name: identifier) =
  let (ImportMap { import_into; symbols; instances }) = im in
  IdentifierMap.find_opt name m

let imported_instances (im: import_map): decl_id list =
  let (ImportMap { instannces; _ }) = im in
  instances
