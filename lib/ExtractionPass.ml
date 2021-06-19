open Type
open Combined
(*open Semantic*)

let rec extract_type_signatures (CombinedModule { decls; _ }) =
  List.filter_map extract_type_signatures' decls

and extract_type_signatures' (def: combined_definition) =
  match def with
  | CConstant _ ->
     None
  | CTypeAlias (_, name, typarams, universe, _, _) ->
     Some (TypeSignature (name, typarams, universe))
  | CRecord (_, name, typarams, universe, _, _) ->
     Some (TypeSignature (name, typarams, universe))
  | CUnion (_, name, typarams, universe, _, _) ->
     Some (TypeSignature (name, typarams, universe))
  | CFunction _ ->
     None
  | CTypeclass _ ->
     None
  | CInstance _ ->
     None
(*
let rec extract_declarations (CombinedModule { decls; _ }) local_types =
  List.filter_map (extract_declaration local_types) decls

and extract_declaration (local_types: type_signature list) (def: combined_definition) =
  match def with
  | CConstant (vis, name, typespec, value, docstring) ->
 *)
