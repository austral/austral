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
  let (CombinedModule {
           name;
           kind;
           interface_file;
           interface_docstring;
           interface_imports;
           body_file;
           body_docstring;
           body_imports;
           decls
      }) = cmodule in
  (* Add the module to the environment. *)
  let input: mod_input = {
      name = name;
      docstring = docstring;
      interface_file = ifile;
      body_file = bfile;
      kind = kind;
    }
  in
  let sigs: type_signature list = extract_type_signatures cmodule in
  let env: env = extract_definitions env mod_id sigs de in
  env

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a list of combined definitions, add all relevant
    decls to the environment. *)
and extract_definitions (env: env) (mod_id: mod_id) (local_types: type_signature list) (defs: combined_definition list): env =
  match defs with
  | first::rest ->
     let env = extract_definition env mod_id first in
     extract_definitions env mod_id rest
  | [] ->
     env

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a combined definition, add all relevant decls to
    the environment. *)
and extract_definition (env: env) (mod_id: mod_id) (local_types: type_signature list) (def: combined_definition): env =
  let parse' = parse_type menv local_types in
  match def with
  | CConstant (vis, name, typespec, _, docstring) ->
     add_constant env { mod_id; vis; name; docstring }
  | CTypeAlias (vis, name, typarams, universe, typespec, docstring) ->
     let rm = region_map_from_typarams typarams in
     let def = parse' rm typarams typespec in
     add_type_alias env { mod_id; vis; name; docstring; typarams; universe; def }
  | CRecord (vis, name, typarams, universe, slots, docstring) ->
     let slots = List.map (parse_slot typarams) slots in
     add_record env { mod_id; vis; name; docstring; typarams; universe; slots }
  | CUnion (vis, name, typarams, universe, cases, docstring) ->
     (* Add the union itself to the env *)
     let (env, union_id) = add_union { mod_id; vis; name; docstring; typarams; universe } in
     (* If the union is public, the cases are public, otherwise they are private. *)
     let case_vis: vis =
       match vis with
       | TypeVisPublic -> VisPublic
       | TypeVisOpaque -> VisPrivate
       | TypeVisPrivate -> Vis Private
     in
     (* Convert the list of cases into a list of union_case_input records *)
     let case_map (QualifiedCase (n, ss)): union_case_input =
       {
         mod_id = mod_id;
         vis = case_vis;
         union_id = union_id;
         name = n;
         slots = List.map (parse_slot typarams) slots;
       }
     in
     let cases: union_case_input list = List.map extract_case cases in
     (* Add the union cases to the environmenmt *)
     add_union_cases env cases
  | CFunction (vis, name, typarams, params, rt, _, docstring, pragmas) ->
     let rm = region_map_from_typarams typarams in
     let value_params = List.map (parse_param typarams) params
     and rt = parse' rm typarams rt in
     let fn_input: function_input = {
         mod_id = mod_id;
         vis = vis;
         name = name;
         docstring = docstring;
         typarams = typarams;
         value_params = value_params;
         rt = rt;
         body = None
       }
     in
     add_function env fn_input
  | CTypeClass (vis, name, typaram, methods, docstring) ->
     (* Add the typeclass itself to the env *)
     let (env, typeclass_id) = add_type_class env { mod_id; vis; name; docstring; param = typaram; } in
     (* Convert the list of methods into a list of type_class_method records *)
     let method_map (CMethodDecl (name, params, rt, docstring)): type_class_method_input =
       let rm = region_map_from_typarams typarams in
       let value_params = List.map (parse_param typarams) params
       and rt = parse' rm typarams rt in
       {
         mod_id;
         vis;
         typeclass_id;
         name;
         docstring;
         value_params;
         rt;
       }
     in
     let methods: type_class_method_input list = List.map method_map methods in
     (* Add the methods to the env *)
     add_type_class_methods env methods
  | CInstance (vis, name, typarams, argument, methods, docstring) ->
     (* Add the instance itself to the env *)
     let rm = region_map_from_typarams typarams in
     let argument = parse' rm typarams argument in
     let typeclass_id: decl_id =
       match get_decl_by_name env name with
       | Some decl ->
          match decl with
          | TypeClass { id; _ } ->
             id
          | _ ->
             err "Type class name refers to something that is not a type class."
       | None ->
          err "Type class with this name does not exist."
     in
     let input: instance_input = { mod_id; typeclass_id; name; docstring; typarams; argument } in
     let (env, instance_id) = add_instance env input in
     (* Convert the list of methods into a list of instance_method_input records *)
     let method_map (CMethodDef (name, params, rt, _, _)): instance_method_input =
       let rm = region_map_from_typarams typarams in
       let value_params = List.map (parse_param typarams) params
       and rt = parse' rm typarams rt in
       {
         instance_id = instance_id;
         docstring = docstring;
         value_params = value_params;
         rt = rt;
         body = None;
       }
     in
     let methods: instance_method_input list = List.map method_map methods in
     (* Add the methods to the env *)
     add_instance_methods env methods

(** Utility functions to add lists of things to the environment. *)
and add_union_cases (env: env) (cases: list union_case_input): env =
  match cases with
  | first::rest ->
     let (env, _) = add_union_case env first in
     add_union_cases env rest
  | [] ->
     env

and add_type_class_methods (env: env) (cases: list type_class_method_input): env =
  match cases with
  | first::rest ->
     let (env, _) = add_type_class_method env first in
     add_type_class_methods env rest
  | [] ->
     env


and add_instance_methods (env: env) (cases: list instance_method_input): env =
  match cases with
  | first::rest ->
     let (env, _) = add_instance_method env first in
     add_instance_methods env rest
  | [] ->
     env
