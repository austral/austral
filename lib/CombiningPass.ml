open Identifier
open Imports
open Qualifier
open AbstractionPass
open Cst
open Combined
open Semantic
open ModuleSystem
open ImportResolution
open Error

let parse_slots (imports: import_map) (slots: concrete_slot list): qslot list =
  List.map
    (fun (ConcreteSlot (n, t)) -> QualifiedSlot (n, qualify_typespec imports t))
    slots

let parse_cases (imports: import_map) (cases: concrete_case list): qcase list =
  List.map
    (fun (ConcreteCase (n, slots)) ->
      QualifiedCase (n,
                     List.map (fun (ConcreteSlot (n, t)) ->
                         QualifiedSlot (n, qualify_typespec imports t))
                       slots))
    cases

let parse_params (imports: import_map) (params: concrete_param list): qparam list =
  List.map
    (fun (ConcreteParam (n, t)) ->
      QualifiedParameter (n, qualify_typespec imports t))
    params

let parse_method_decls (imports: import_map) (methods: concrete_method_decl list): combined_method_decl list =
  List.map (fun (ConcreteMethodDecl (n, params, rt, method_docstring)) ->
      CMethodDecl (n,
                   parse_params imports params,
                   qualify_typespec imports rt,
                   method_docstring))
    methods

let parse_method_defs (imports: import_map) (methods: concrete_method_def list): combined_method_def list =
  List.map (fun (ConcreteMethodDef (n, params, rt, body, method_docstring)) ->
      CMethodDef (n,
                  parse_params imports params,
                  qualify_typespec imports rt,
                  method_docstring,
                  abs_stmt imports body))
    methods

(* Given an interface declaration, and a body declaration, return the combined
   declaration if they're the same, and fail otherwise. *)
let match_decls (ii: import_map) (bi: import_map) (decl: concrete_decl) (def: concrete_def): combined_definition =
  match decl with
  | ConcreteConstantDecl (name, ty, docstring) ->
     (match def with
      | ConcreteConstantDef (name', ty', value, _) ->
         if (name = name') && (ty = ty') then
           CConstant (VisPublic, name, qualify_typespec ii ty, abs_expr bi value, docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a constant")
  | ConcreteOpaqueTypeDecl (name, typarams, universe, docstring) ->
     (match def with
      | ConcreteTypeAliasDef (ConcreteTypeAlias (name', typarams', universe', ty, _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') then
           CTypeAlias (TypeVisOpaque, name, typarams, universe, qualify_typespec bi ty, docstring)
         else
           err "Mismatch"
      | ConcreteRecordDef (ConcreteRecord (name', typarams', universe', slots, _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') then
           CRecord (TypeVisOpaque,
                    name,
                    typarams,
                    universe,
                    parse_slots bi slots,
                    docstring)
         else
           err "Mismatch"
      | ConcreteUnionDef (ConcreteUnion (name', typarams', universe', cases, _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') then
           CUnion (TypeVisOpaque,
                   name,
                   typarams,
                   universe,
                   parse_cases bi cases,
                   docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a type")
  | ConcreteTypeAliasDecl (ConcreteTypeAlias (name, typarams, universe, ty, docstring)) ->
     (match def with
      | ConcreteTypeAliasDef (ConcreteTypeAlias (name', typarams', universe', ty', _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') && (ty = ty') then
           CTypeAlias (TypeVisPublic, name, typarams, universe, qualify_typespec ii ty, docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a type alias")
  | ConcreteRecordDecl (ConcreteRecord (name, typarams, universe, slots, docstring)) ->
     (match def with
      | (ConcreteRecordDef (ConcreteRecord (name', typarams', universe', slots', _))) ->
         if (name = name') && (typarams = typarams') && (universe = universe') && (slots = slots') then
           CRecord (TypeVisPublic,
                    name,
                    typarams,
                    universe,
                    parse_slots ii slots,
                    docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a record")
  | ConcreteUnionDecl (ConcreteUnion (name, typarams, universe, cases, docstring)) ->
     (match def with
      | (ConcreteUnionDef (ConcreteUnion (name', typarams', universe', cases', _))) ->
         if (name = name') && (typarams = typarams') && (universe = universe') && (cases = cases') then
           CUnion (TypeVisPublic,
                   name,
                   typarams,
                   universe,
                   parse_cases ii cases,
                   docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a union")
  | ConcreteFunctionDecl (name, typarams, params, rt, docstring) ->
     (match def with
      | ConcreteFunctionDef (name', typarams', params', rt', body, _, pragmas) ->
         if (name = name') && (typarams = typarams') && (params == params') && (rt == rt') then
           CFunction (VisPublic,
                      name,
                      typarams,
                      parse_params ii params,
                      qualify_typespec ii rt,
                      abs_stmt bi body,
                      docstring,
                      pragmas)
         else
           err "Mismatch"
      | _ ->
         err "Not a function")
  | ConcreteTypeClassDecl (ConcreteTypeClass (name, typaram, methods, docstring)) ->
     (match def with
      | ConcreteTypeClassDef (ConcreteTypeClass (name', typaram', methods', _)) ->
         if (name = name') && (typaram = typaram') && (methods = methods') then
           CTypeclass (VisPublic,
                       name,
                       typaram,
                       parse_method_decls ii methods,
                       docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a type class")
  | ConcreteInstanceDecl (name, typarams, argument, docstring) ->
     (match def with
      | ConcreteInstanceDef (ConcreteInstance (name', typarams', argument', methods, _)) ->
         if (name = name') && (typarams = typarams') && (argument = argument') then
           CInstance (VisPublic,
                      name,
                      typarams,
                      qualify_typespec ii argument,
                      parse_method_defs bi methods,
                      docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not an instance")

let private_def im def =
  match def with
  | ConcreteConstantDef (name, ty, value, docstring) ->
     CConstant (VisPrivate,
                name,
                qualify_typespec im ty,
                abs_expr im value,
                docstring)
  | ConcreteTypeAliasDef (ConcreteTypeAlias (name, typarams, universe, ty, docstring)) ->
     CTypeAlias (TypeVisPrivate,
                 name,
                 typarams,
                 universe,
                 qualify_typespec im ty,
                 docstring)
  | ConcreteRecordDef (ConcreteRecord (name, typarams, universe, slots, docstring)) ->
     CRecord (TypeVisPrivate,
              name,
              typarams,
              universe,
              parse_slots im slots,
              docstring)
  | ConcreteUnionDef (ConcreteUnion (name, typarams, universe, cases, docstring)) ->
     CUnion (TypeVisPrivate,
             name,
             typarams,
             universe,
             parse_cases im cases,
             docstring)
  | ConcreteFunctionDef (name, typarams, params, rt, body, docstring, pragmas) ->
     CFunction (VisPrivate,
                name,
                typarams,
                parse_params im params,
                qualify_typespec im rt,
                abs_stmt im body,
                docstring,
                pragmas)
  | ConcreteTypeClassDef (ConcreteTypeClass (name, typaram, methods, docstring)) ->
     CTypeclass (VisPrivate,
                 name,
                 typaram,
                 parse_method_decls im methods,
                 docstring)
  | ConcreteInstanceDef (ConcreteInstance (name, typarams, argument, methods, docstring)) ->
     CInstance (VisPrivate,
                name,
                typarams,
                qualify_typespec im argument,
                parse_method_defs im methods,
                docstring)

let rec combine (menv: menv) (cmi: concrete_module_interface) (cmb: concrete_module_body): combined_module =
  let (ConcreteModuleInterface (mn, interface_imports, decls)) = cmi
  and (ConcreteModuleBody (mn', body_imports, defs)) = cmb
  in
  if mn != mn' then
    err ("Interface and body have mismatching names: "
         ^ (mod_name_string mn)
         ^ " and "
         ^ (mod_name_string mn'))
  else
    let im = resolve mn menv interface_imports
    and bm = resolve mn menv body_imports
    in
    let public_decls = List.map (parse_decl im bm cmb) decls
    and private_decls = parse_defs cmi im defs
    in
    CombinedModule {
        name = mn;
        interface_imports = im;
        body_imports = bm;
        decls = List.concat [public_decls; private_decls];
      }

and parse_decl (im: import_map) (bm: import_map) (cmb: concrete_module_body) (decl: concrete_decl): combined_definition =
  match decl_name decl with
  | (Some name) ->
     (match get_concrete_def cmb name with
      | (Some def) ->
         match_decls im bm decl def
      | None ->
         err "Interface declaration has no corresponding body declaration")
  | None ->
     (match decl with
      | ConcreteInstanceDecl (name, typarams, argument, _) ->
         (* It's an instance declaration. Find the corresponding instance in the body. *)
         (match get_instance_def cmb name typarams argument with
          | (Some def) ->
             match_decls im bm decl (ConcreteInstanceDef def)
          | None ->
             err "Interface declaration has no corresponding body declaration")
      | _ ->
         err "Internal")

and parse_defs (cmi: concrete_module_interface) (im: import_map) (defs: concrete_def list): combined_definition list =
  List.filter_map (parse_def cmi im) defs

and parse_def (cmi: concrete_module_interface) (im: import_map) (def: concrete_def): combined_definition option =
  match def_name def with
  | (Some name) ->
     (* If this def exists in the interface, skip it *)
     (match get_concrete_decl cmi name with
      | (Some _) ->
         None
      | None ->
         Some (private_def im def))
  | None ->
     (match def with
      | ConcreteInstanceDef (ConcreteInstance (name, typarams, argument, _, _)) ->
      (* It's an instance declaration. If the interface file declares it, ignore
         it: it's already been processed. Otherwise, process it as a private
         instance declaration. *)
         if has_instance_decl cmi name typarams argument then
           None
         else
           Some (private_def im def)
      | _ ->
         err "Internal")
