open Identifier
open IdentifierMap
open Semantic

type import_map = qident IdentifierMap.t * semantic_typeclass list * semantic_instance list
