open Identifier
open IdentifierSet
open Common
open Type
open TypeSystem
open Combined
open Linked
open Ast
open Id
open Env
open TypeParser
open TypeSignature
open TypeParameters
open Util
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

let rec extract (env: env) (cmodule: combined_module) (interface_file: file_id) (body_file: file_id): (env * linked_module) =
  let (CombinedModule {
           name;
           kind;
           interface_docstring;
           interface_imports;
           body_docstring;
           body_imports;
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
  let (env, linked_defs): (env * linked_definition list) = extract_definitions env mod_id sigs decls in
  (* Construct the linked module *)
  let lmod = LinkedModule {
                 mod_id = mod_id;
                 name = name;
                 kind = kind;
                 interface_docstring = interface_docstring;
                 interface_imports = interface_imports;
                 body_docstring = body_docstring;
                 body_imports = body_imports;
                 decls = linked_defs
               }
  in
  (env, lmod)

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a list of combined definitions, add all relevant
    decls to the environment, and return all corresponding linked decls. *)
and extract_definitions (env: env) (mod_id: mod_id) (local_types: type_signature list) (defs: combined_definition list): (env * (linked_definition list)) =
  let f ((env, comb_def): (env * combined_definition)): (env * linked_definition) =
    extract_definition env mod_id local_types comb_def
  in
  map_with_context f env defs

(** Given the environment, the ID of the module we're extracting, the list of
    local type signatures, and a combined definition, add all relevant decls to
    the environment, and return a linked definition. *)
and extract_definition (env: env) (mod_id: mod_id) (local_types: type_signature list) (def: combined_definition): (env * linked_definition)  =
  let parse' = parse_type env local_types in
  let rec parse_slot (typarams: typarams) (QualifiedSlot (n, ts)): typed_slot =
    let rm = region_map_from_typarams typarams in
    TypedSlot (n, parse' rm typarams ts)
  and parse_param (typarams: typarams) (QualifiedParameter (n, ts)): value_parameter =
    let rm = region_map_from_typarams typarams in
    ValueParameter (n, parse' rm typarams ts)
  in
  match def with
  | CConstant (vis, name, typespec, def, docstring) ->
     let rm = region_map_from_typarams empty_typarams in
     let ty = parse' rm empty_typarams typespec in
     let (env, decl_id) = add_constant env { mod_id; vis; name; ty; docstring } in
     let decl = LConstant (decl_id, vis, name, ty, def, docstring) in
     (env, decl)
  | CTypeAlias (vis, name, typarams, universe, typespec, docstring) ->
     let rm = region_map_from_typarams typarams in
     let def = parse' rm typarams typespec in
     let (env, decl_id) = add_type_alias env { mod_id; vis; name; docstring; typarams; universe; def } in
     let decl = LTypeAlias (decl_id, vis, name, typarams, universe, def, docstring) in
     (env, decl)
  | CRecord (vis, name, typarams, universe, slots, docstring) ->
     let slots = List.map (parse_slot typarams) slots in
     let (env, decl_id) = add_record env { mod_id; vis; name; docstring; typarams; universe; slots } in
     let decl = LRecord (decl_id, vis, name, typarams, universe, slots, docstring) in
     (env, decl)
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
     let (env, linked_cases) = add_union_cases env cases in
     (* Construct the decl *)
     let decl = LUnion (union_id, vis, name, typarams, universe, linked_cases, docstring) in
     (env, decl)
  | CFunction (vis, name, typarams, params, rt, body, docstring, pragmas) ->
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
     let (env, decl_id) = add_function env fn_input in
     let decl = LFunction (decl_id, vis, name, typarams, value_params, rt, body, docstring, pragmas) in
     (env, decl)
  | CTypeclass (vis, name, typaram, methods, docstring) ->
     (* Add the typeclass itself to the env *)
     let (env, typeclass_id) = add_type_class env { mod_id; vis; name; docstring; param = typaram; } in
     (* Convert the list of methods into a list of type_class_method records *)
     let typarams = typarams_from_list [typaram] in
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
     let (env, linked_methods) = add_type_class_methods env methods in
     (* Construct the decl *)
     let decl = LTypeclass (typeclass_id, vis, name, typaram, linked_methods, docstring) in
     (env, decl)
  | CInstance (vis, name, typarams, CombinedInstanceArg (arg_name, arg_args), methods, docstring) ->
     (* First we add the typeclass, then we add the methods. *)
     (* Find the ID of the typeclass. *)
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
     (* Check that the set of type variables applied to the arguments is
        identical to the set of type parameters.

        When we have a generic instance, the shape of the argument is T[A_0,
        ..., A_n], and the typaram set must likewise be [A_0, ..., A_n].  *)
     let _ =
       (* Convert typarams to an identifier set. *)
       let typaram_names: identifier list =
         List.map (fun (TypeParameter (name, _, _)) -> name) (typarams_as_list typarams)
       in
       let typarams_set: IdentifierSet.t = IdentifierSet.of_list typaram_names
         (* Convert args to an identifier set *)
       and arg_set: IdentifierSet.t = IdentifierSet.of_list arg_args
       in
       (* Compare sets for equality *)
       if IdentifierSet.equal typarams_set arg_set then
         ()
       else
         err "When defining a generic typeclass instance, the set of type parameters must be the same as the set of type arguments."
     in
     (* Find the argument's type signature. *)
     let signature: type_signature = get_type_signature_by_name env (qident_to_sident arg_name) in
     (* Check that the argument type's parameter list fits ours. *)
     (* Check the universes match *)
     let input: instance_input = { mod_id; vis; typeclass_id; docstring; typarams; argument } in
     let (env, instance_id) = add_instance env input in
     (* Convert the list of methods into a list of instance_method_input records *)
     let method_map (CMethodDef (name, params, rt, meth_docstring, body)): (instance_method_input * astmt) =
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
       ({
           instance_id = instance_id;
           method_id = method_id;
           docstring = meth_docstring;
           name = name;
           value_params = value_params;
           rt = rt;
           body = None;
         },
        body)
     in
     let methods: (instance_method_input * astmt) list = List.map method_map methods in
     (* Add the methods to the env *)
     let (env, linked_methods) = add_instance_methods env methods in
     (* Construct the decl *)
     let decl = LInstance (instance_id, vis, name, typarams, argument, linked_methods, docstring) in
     (env, decl)

(** Utility functions to add lists of things to the environment. *)
and add_union_cases (env: env) (cases: union_case_input list): (env * (linked_case list)) =
  let f ((env, input): (env * union_case_input)): (env * linked_case) =
    let (env, decl_id) = add_union_case env input in
    let { name; slots; _ } = input in
    let case = LCase (decl_id, name, slots) in
    (env, case)
  in
  map_with_context f env cases

and add_type_class_methods (env: env) (cases: type_class_method_input list): (env * (linked_method_decl list)) =
  let f ((env, input): (env * type_class_method_input)): (env * linked_method_decl) =
    let (env, decl_id) = add_type_class_method env input in
    let { name; value_params; rt; docstring; vis; _ } = input in
    (* LOAD-BEARING HACK: need to destructure `vis` because otherwise the
       compiler thinks `input` is supposed to be an instance of
       `instance_method_input` (???). *)
    let _ = vis in
    let decl = LMethodDecl (decl_id, name, value_params, rt, docstring) in
    (env, decl)
  in
  map_with_context f env cases

and add_instance_methods (env: env) (cases: (instance_method_input * astmt) list): (env * (linked_method_def list)) =
  let f ((env, pair): (env * (instance_method_input * astmt))): (env * linked_method_def) =
    let (input, body) = pair in
    let (env, decl_id) = add_instance_method env input in
    let { name; value_params; rt; docstring; _ } = input in
    let decl = LMethodDef (decl_id, name, value_params, rt, docstring, body) in
    (env, decl)
  in
  map_with_context f env cases
