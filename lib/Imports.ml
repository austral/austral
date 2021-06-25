open Identifier
open IdentifierMap
open Semantic

type import_map =
  ImportMap of module_name * qident IdentifierMap.t * semantic_typeclass list * semantic_instance list

let empty_map n =
  ImportMap (n, IdentifierMap.empty, [], [])

let add_symbol (ImportMap (n, m, cs, is)) q =
  ImportMap (n, IdentifierMap.add (local_name q) q m, cs, is)

let importing_module (ImportMap (n, _, _, _)) =
  n

let get_symbol (ImportMap (_, m, _, _)) name =
  IdentifierMap.find_opt name m

let imported_classes (ImportMap (_, _, cs, _)) =
  cs

let imported_instances (ImportMap (_, _, _, is)) =
  is
