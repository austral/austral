open Identifier
open Common
open Type
open TypeSystem
open Combined
open Env
open TypeParser
open Error

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
           body_file;
           body_docstring;
           decls;
           _
      }) = cmodule in
  (* Add the module to the environment. *)
  let input: mod_input = {
      name;
      interface_file;
      interface_docstring;
      body_file;
      body_docstring;
      kind
    }
  in
  let (env, mod_id) = add_module env input in
  (* Extract the list of local type signatures. This lets us parse type
     specifiers without reference to the environment. *)
  let sigs: type_signature list = extract_type_signatures cmodule in
  (* Extract all declarations from the module into the environment. *)
  let env: env = extract_definitions env mod_id sigs decls in
  env

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a list of combined definitions, add all relevant
    decls to the environment. *)
and extract_definitions (env: env) (mod_id: mod_id) (local_types: type_signature list) (defs: combined_definition list): env =
  match defs with
  | first::rest ->
     let env = extract_definition env mod_id local_types first in
     extract_definitions env mod_id local_types rest
  | [] ->
     env

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a combined definition, add all relevant decls to
    the environment. *)
and extract_definition (env: env) (mod_id: mod_id) (local_types: type_signature list) (def: combined_definition): env =
  let parse' = parse_type env local_types in
  let rec parse_slot (typarams: type_parameter list) (QualifiedSlot (n, ts)): typed_slot =
    let rm = region_map_from_typarams typarams in
    TypedSlot (n, parse' rm typarams ts)
  and parse_param (typarams: type_parameter list) (QualifiedParameter (n, ts)): value_parameter =
    let rm = region_map_from_typarams typarams in
    ValueParameter (n, parse' rm typarams ts)
  in
  match def with
  | CConstant (vis, name, typespec, _, docstring) ->
     let rm = region_map_from_typarams [] in
     let ty = parse' rm [] typespec in
     let (env, _) = add_constant env { mod_id; vis; name; ty; docstring } in
     env
  | CTypeAlias (vis, name, typarams, universe, typespec, docstring) ->
     let rm = region_map_from_typarams typarams in
     let def = parse' rm typarams typespec in
     let (env, _) = add_type_alias env { mod_id; vis; name; docstring; typarams; universe; def } in
     env
  | CRecord (vis, name, typarams, universe, slots, docstring) ->
     let slots = List.map (parse_slot typarams) slots in
     let (env, _) = add_record env { mod_id; vis; name; docstring; typarams; universe; slots } in
     env
  | CUnion (vis, name, typarams, universe, cases, docstring) ->
     (* Add the union itself to the env *)
     let (env, union_id) = add_union env { mod_id; vis; name; docstring; typarams; universe } in
     (* If the union is public, the cases are public, otherwise they are private. *)
     let case_vis: vis =
       match vis with
       | TypeVisPublic -> VisPublic
       | TypeVisOpaque -> VisPrivate
       | TypeVisPrivate -> VisPrivate
     in
     (* Convert the list of cases into a list of union_case_input records *)
     let case_map (QualifiedCase (n, slots)): union_case_input =
       let docstring = Docstring "" in (* TODO: Store docstrings in slots and cases *)
       {
         mod_id = mod_id;
         vis = case_vis;
         union_id = union_id;
         name = n;
         slots = List.map (parse_slot typarams) slots;
         docstring = docstring;
       }
     in
     let cases: union_case_input list = List.map case_map cases in
     (* Add the union cases to the environmenmt *)
     add_union_cases env cases
  | CFunction (vis, name, typarams, params, rt, _, docstring, pragmas) ->
     let rm = region_map_from_typarams typarams in
     let value_params = List.map (parse_param typarams) params
     and rt = parse' rm typarams rt
     and external_name: string option =
       (match pragmas with
        | [ForeignImportPragma s] ->
           Some s
        | _ ->
           None)
    in
     let fn_input: function_input = {
         mod_id = mod_id;
         vis = vis;
         name = name;
         docstring = docstring;
         typarams = typarams;
         value_params = value_params;
         rt = rt;
         external_name = external_name;
         body = None
       }
     in
     let (env, _) = add_function env fn_input in
     env
  | CTypeclass (vis, name, typaram, methods, docstring) ->
     (* Add the typeclass itself to the env *)
     let (env, typeclass_id) = add_type_class env { mod_id; vis; name; docstring; param = typaram; } in
     (* Convert the list of methods into a list of type_class_method records *)
     let typarams = [typaram] in
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
       match get_decl_by_name env (qident_to_sident name) with
       | Some decl ->
          (match decl with
           | TypeClass { id; _ } ->
              id
           | _ ->
              err "Type class name refers to something that is not a type class.")
       | None ->
          err "Type class with this name does not exist."
     in
     let input: instance_input = { mod_id; vis; typeclass_id; docstring; typarams; argument } in
     let (env, instance_id) = add_instance env input in
     (* Convert the list of methods into a list of instance_method_input records *)
     let method_map (CMethodDef (name, params, rt, _, _)): instance_method_input =
       let rm = region_map_from_typarams typarams in
       let value_params = List.map (parse_param typarams) params
       and rt = parse' rm typarams rt
       and method_id: decl_id =
         (match get_method_from_typeclass_id_and_name env typeclass_id name with
          | Some (TypeClassMethod { id; _ }) ->
             id
          | _ ->
             err "No method with this name.")
       in
       {
         instance_id = instance_id;
         method_id = method_id;
         docstring = docstring;
         name = name;
         value_params = value_params;
         rt = rt;
         body = None;
       }
     in
     let methods: instance_method_input list = List.map method_map methods in
     (* Add the methods to the env *)
     add_instance_methods env methods

(** Utility functions to add lists of things to the environment. *)
and add_union_cases (env: env) (cases: union_case_input list): env =
  match cases with
  | first::rest ->
     let (env, _) = add_union_case env first in
     add_union_cases env rest
  | [] ->
     env

and add_type_class_methods (env: env) (cases: type_class_method_input list): env =
  match cases with
  | first::rest ->
     let (env, _) = add_type_class_method env first in
     add_type_class_methods env rest
  | [] ->
     env

and add_instance_methods (env: env) (cases: instance_method_input list): env =
  match cases with
  | first::rest ->
     let (env, _) = add_instance_method env first in
     add_instance_methods env rest
  | [] ->
     env
