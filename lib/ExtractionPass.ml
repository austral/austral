open Identifier
open Imports
open Type
open TypeSystem
open Region
open Combined
open Env
open TypeParser

let rec extract_type_signatures (CombinedModule { decls; _ }): type_signature list =
  List.filter_map extract_type_signatures' decls

and extract_type_signatures' (def: combined_definition): type_signature option =
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

let rec extract (env: env) (cmodule: combined_module): env =
  let (CombinedModule { name; interface_imports; body_imports; _ }) = cmodule in
  let sigs: type_signature list = extract_type_signatures cmodule in
  let env: env = extract_declarations env sigs cmodule in
  env

let rec extract_declarations (env: env) local_types (CombinedModule { name; decls; _ }): decl list =
  List.map (extract_declaration module_name menv local_types) decls

and extract_declaration (module_name: module_name) (env: env) (local_types: type_signature list) (def: combined_definition): decl list =
  let parse' = parse_type menv local_types in
  let rec parse_slot (typarams: type_parameter list) (QualifiedSlot (n, ts)): typed_slot =
    let rm = region_map_from_typarams typarams in
    TypedSlot (n, parse' rm typarams ts)
  and parse_case (typarams: type_parameter list) (QualifiedCase (n, ss)): typed_case =
    TypedCase (n, List.map (parse_slot typarams) ss)
  and parse_param (typarams: type_parameter list) (QualifiedParameter (n, ts)): value_parameter =
    let rm = region_map_from_typarams typarams in
    ValueParameter (n, parse' rm typarams ts)
  and parse_method_decl (typarams: type_parameter list) (CMethodDecl (name, params, rt, _)): semantic_method_decl =
    let rm = region_map_from_typarams typarams in
    SMethodDecl (name, List.map (parse_param typarams) params, parse' rm typarams rt, None)
  and parse_method_def (typarams: type_parameter list) (CMethodDef (name, params, rt, _, _)): semantic_method_decl =
    let rm = region_map_from_typarams typarams in
    SMethodDecl (name, List.map (parse_param typarams) params, parse' rm typarams rt, None)
  in
  match def with
  | CConstant (vis, name, typespec, _, _) ->
     SConstantDefinition (vis, name, parse' empty_region_map [] typespec)
  | CTypeAlias (vis, name, typarams, universe, typespec, _) ->
     let rm = region_map_from_typarams typarams in
     STypeAliasDefinition (vis, name, typarams, universe, parse' rm typarams typespec)
  | CRecord (vis, name, typarams, universe, slots, _) ->
     SRecordDefinition (module_name, vis, name, typarams, universe, List.map (parse_slot typarams) slots)
  | CUnion (vis, name, typarams, universe, cases, _) ->
     SUnionDefinition (module_name, vis, name, typarams, universe, List.map (parse_case typarams) cases)
  | CFunction (vis, name, typarams, params, rt, _, _, _) ->
     let rm = region_map_from_typarams typarams in
     SFunctionDeclaration (vis, name, typarams, List.map (parse_param typarams) params, parse' rm typarams rt, None)
  | CTypeclass (vis, name, typaram, methods, _) ->
     STypeClassDecl (STypeClass (vis, name, typaram, List.map (parse_method_decl [typaram]) methods))
  | CInstance (vis, name, typarams, argument, methods, _) ->
     let rm = region_map_from_typarams typarams in
     STypeClassInstanceDecl (STypeClassInstance (vis, name, typarams, parse' rm typarams argument, List.map (parse_method_def typarams) methods))

(** Given the environment, the ID of the module we're extracting, and a combined
    definition, add all relevant decls to the environment. *)
let extract_definition (env: env) (mod_id: mod_id) (def: combined_definition): env =
  match def with
  | CConstant (vis, name, typespec, _, docstring) ->
     add_constant env { mod_id; vis; name; docstring }
  | CTypeAlias (vis, name, typarams, universe, typespec, docstring) ->
     let rm = region_map_from_typarams typarams in
     let def = parse' rm typarams typespec in
     add_type_alias env { mod_id; vis; name; docstring; typarams; universe; def }
  | CRecord (vis, name, typarams, universe, slots, _) ->
     let slots = List.map (parse_slot typarams) slots in
     add_record env { mod_id; vis; name; docstrings; typarams; universe; slots }
