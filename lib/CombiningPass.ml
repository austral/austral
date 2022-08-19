open Identifier
open Imports
open Qualifier
open TypeParameter
open TypeParameters
open AbstractionPass
open Cst
open CstUtil
open Combined
open Env
open ImportResolution
open Reporter
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

let name_typarams (im: import_map) (params: concrete_type_param list) (name: qident): typarams =
  let lst: type_parameter list =
    List.map (fun (ConcreteTypeParam (n, u, cs)) -> make_typaram (n, u, name, List.map (fun i -> qident_to_sident (qualify_identifier im i)) cs)) params
  in
  typarams_from_list lst

let parse_method_decls (module_name: module_name) (imports: import_map) (methods: concrete_method_decl list): combined_method_decl list =
  List.map (fun (ConcreteMethodDecl (n, typarams, params, rt, method_docstring)) ->
      CMethodDecl (n,
                   name_typarams imports typarams (make_qident (module_name, n, n)),
                   parse_params imports params,
                   qualify_typespec imports rt,
                   method_docstring))
    methods

let parse_method_defs (module_name: module_name) (imports: import_map) (methods: concrete_method_def list): combined_method_def list =
  List.map (fun (ConcreteMethodDef (n, typarams, params, rt, body, method_docstring)) ->
      CMethodDef (n,
                  name_typarams imports typarams (make_qident (module_name, n, n)),
                  parse_params imports params,
                  qualify_typespec imports rt,
                  method_docstring,
                  abs_stmt imports body))
    methods

let match_decls (module_name: module_name) (ii: import_map) (bi: import_map) (decl: concrete_decl) (def: concrete_def): combined_definition =
  let make_qname n =
    make_qident (module_name, n, n)
  in
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
      | ConcreteRecordDef (ConcreteRecord (name', typarams', universe', slots, _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') then
           let qname = make_qname name' in
           CRecord (TypeVisOpaque,
                    name,
                    name_typarams bi typarams qname,
                    universe,
                    parse_slots bi slots,
                    docstring)
         else
           err "Mismatch"
      | ConcreteUnionDef (ConcreteUnion (name', typarams', universe', cases, _)) ->
         if (name = name') && (typarams = typarams') && (universe = universe') then
           let qname = make_qname name' in
           CUnion (TypeVisOpaque,
                   name,
                   name_typarams bi typarams qname,
                   universe,
                   parse_cases bi cases,
                   docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not a type")
  | ConcreteFunctionDecl (name, typarams, params, rt, docstring) ->
     (match def with
      | ConcreteFunctionDef (name', typarams', params', rt', body, _, pragmas) ->
         if (name = name') && (typarams = typarams') && (params = params') && (rt = rt') then
           let qname = make_qname name' in
           CFunction (VisPublic,
                      name,
                      name_typarams bi typarams qname,
                      parse_params ii params,
                      qualify_typespec ii rt,
                      abs_stmt bi body,
                      docstring,
                      pragmas)
         else
           err "Function declaration does not match definition"
      | _ ->
         err "Not a function")
  | ConcreteInstanceDecl (name, typarams, argument, docstring) ->
     (match def with
      | ConcreteInstanceDef (ConcreteInstance (name', typarams', argument', methods, _)) ->
         if (name = name') && (typarams = typarams') && (argument = argument') then
           (* Instance names might refer to an imported typeclass, so we have to
              qualify them. Since we're parsing a public declaration, which
              means the instance (and thus the name of the typeclass) appears in
              the interface file, we use the interface imports for
              qualification. *)
           let qname = qualify_identifier ii name in
           CInstance (VisPublic,
                      qname,
                      name_typarams bi typarams qname,
                      qualify_typespec ii argument,
                      parse_method_defs module_name bi methods,
                      docstring)
         else
           err "Mismatch"
      | _ ->
         err "Not an instance")
  | _ ->
     err "Invalid decl in this context"

let private_def module_name im def =
  let make_qname n =
    make_qident (module_name, n, n)
  in
  match def with
  | ConcreteConstantDef (name, ty, value, docstring) ->
     CConstant (VisPrivate,
                name,
                qualify_typespec im ty,
                abs_expr im value,
                docstring)
  | ConcreteRecordDef (ConcreteRecord (name, typarams, universe, slots, docstring)) ->
     let qname = make_qname name in
     CRecord (TypeVisPrivate,
              name,
              name_typarams im typarams qname,
              universe,
              parse_slots im slots,
              docstring)
  | ConcreteUnionDef (ConcreteUnion (name, typarams, universe, cases, docstring)) ->
     let qname = make_qname name in
     CUnion (TypeVisPrivate,
             name,
             name_typarams im typarams qname,
             universe,
             parse_cases im cases,
             docstring)
  | ConcreteFunctionDef (name, typarams, params, rt, body, docstring, pragmas) ->
     let qname = make_qname name in
     CFunction (VisPrivate,
                name,
                name_typarams im typarams qname,
                parse_params im params,
                qualify_typespec im rt,
                abs_stmt im body,
                docstring,
                pragmas)
  | ConcreteTypeClassDef (ConcreteTypeClass (name, typaram, methods, docstring)) ->
     let qname = make_qname name in
     CTypeclass (VisPrivate,
                 name,
                 (match (typarams_as_list (name_typarams im [typaram] qname)) with
                  | [tp] ->
                     tp
                  | _ ->
                     err "typeclass has more than one parameter"),
                 parse_method_decls module_name im methods,
                 docstring)
  | ConcreteInstanceDef (ConcreteInstance (name, typarams, argument, methods, docstring)) ->
     (* Instance names might refer to an imported typeclass, so we have to
        qualify them. Since we're parsing a private declaration, which means the
        instance (and thus the name of the typeclass) appears in the body file,
        we can use the body imports for qualification. *)
     let qname = qualify_identifier im name in
     CInstance (VisPrivate,
                qname,
                name_typarams im typarams qname,
                qualify_typespec im argument,
                parse_method_defs module_name im methods,
                docstring)

let rec combine (env: env) (cmi: concrete_module_interface) (cmb: concrete_module_body): combined_module =
  with_frame "Module combining pass: combining .aui and .aum files"
    (fun _ ->
      let (ConcreteModuleInterface (mn, interface_docstring, interface_imports, decls)) = cmi
      and (ConcreteModuleBody (mn', kind, body_docstring, body_imports, defs)) = cmb
      in
      ps ("Module name", (mod_name_string mn));
      if mn <> mn' then
        err ("Interface and body have mismatching names: "
             ^ (mod_name_string mn)
             ^ " and "
             ^ (mod_name_string mn'))
      else
        let im = resolve mn kind env interface_imports
        and bm = resolve mn kind env body_imports
        in
        let public_decls = List.map (parse_decl mn im bm cmb) decls
        and private_decls = parse_defs mn cmi bm defs
        in
        CombinedModule {
            name = mn;
            kind = kind;
            interface_docstring = interface_docstring;
            interface_imports = im;
            body_docstring = body_docstring;
            body_imports = bm;
            decls = List.concat [public_decls; private_decls];
    })

and parse_decl (module_name: module_name) (im: import_map) (bm: import_map) (cmb: concrete_module_body) (decl: concrete_decl): combined_definition =
  let make_qname n =
    make_qident (module_name, n, n)
  in
  match concrete_decl_name decl with
  | (Some name) ->
     (match decl with
      (* Some declarations don't need to have a matching body *)
      | ConcreteRecordDecl (ConcreteRecord (name, typarams, universe, slots, docstring)) ->
         let qname = make_qname name in
         CRecord (TypeVisPublic,
                  name,
                  name_typarams im typarams qname,
                  universe,
                  parse_slots im slots,
                  docstring)
      | ConcreteUnionDecl (ConcreteUnion (name, typarams, universe, cases, docstring)) ->
         let qname = make_qname name in
         CUnion (TypeVisPublic,
                 name,
                 name_typarams im typarams qname,
                 universe,
                 parse_cases im cases,
                 docstring)
      | ConcreteTypeClassDecl (ConcreteTypeClass (name, typaram, methods, docstring)) ->
         let qname = make_qname name in
         CTypeclass (VisPublic,
                     name,
                 (match (typarams_as_list (name_typarams im [typaram] qname)) with
                  | [tp] ->
                     tp
                  | _ ->
                     err "typeclass has more than one parameter"),
                     parse_method_decls module_name im methods,
                     docstring)
      | _ ->
         (* Other declarations need to match a body *)
         (match get_concrete_def cmb name with
          | (Some def) ->
             match_decls module_name im bm decl def
          | None ->
             err "Interface declaration has no corresponding body declaration"))
  | None ->
     (match decl with
      | ConcreteInstanceDecl (name, typarams, argument, _) ->
         (* It's an instance declaration. Find the corresponding instance in the body. *)
         (match get_instance_def cmb name typarams argument with
          | (Some def) ->
             match_decls module_name im bm decl (ConcreteInstanceDef def)
          | None ->
             err "Interface declaration has no corresponding body declaration")
      | _ ->
         err "Internal")

and parse_defs (mn: module_name) (cmi: concrete_module_interface) (im: import_map) (defs: concrete_def list): combined_definition list =
  List.filter_map (parse_def mn cmi im) defs

and parse_def (module_name: module_name) (cmi: concrete_module_interface) (im: import_map) (def: concrete_def): combined_definition option =
  match def_name def with
  | (Some name) ->
     (* If this def exists in the interface, skip it *)
     (match get_concrete_decl cmi name with
      | (Some _) ->
         None
      | None ->
         Some (private_def module_name im def))
  | None ->
     (match def with
      | ConcreteInstanceDef (ConcreteInstance (name, typarams, argument, _, _)) ->
      (* It's an instance declaration. If the interface file declares it, ignore
         it: it's already been processed. Otherwise, process it as a private
         instance declaration. *)
         if has_instance_decl cmi name typarams argument then
           None
         else
           Some (private_def module_name im def)
      | _ ->
         err "Internal")

let as_public (def: combined_definition): combined_definition =
  match def with
  | CConstant (_, name, ty, value, docstring) ->
     CConstant (VisPublic, name, ty, value, docstring)
  | CRecord (_, name, typarams, universe, slots, docstring) ->
     CRecord (TypeVisPublic, name, typarams, universe, slots, docstring)
  | CUnion (_, name, typarams, universe, cases, docstring) ->
     CUnion (TypeVisPublic, name, typarams, universe, cases, docstring)
  | CFunction (_, name, typarams, params, rt, body, docstring, pragmas) ->
     CFunction (VisPublic, name, typarams, params, rt, body, docstring, pragmas)
  | CTypeclass (_, name, typaram, methods, docstring) ->
     CTypeclass (VisPublic, name, typaram, methods, docstring)
  | CInstance (_, name, typarams, argument, methods, docstring) ->
     CInstance (VisPublic, name, typarams, argument, methods, docstring)

let body_as_combined (env: env) (body: concrete_module_body): combined_module =
  with_frame "Module combining pass: module body without interface"
    (fun _ ->
      let (ConcreteModuleBody (mn, kind, body_docstring, body_imports, defs)) = body in
      ps ("Module name", (mod_name_string mn));
      (* We fake an empty module interface. *)
      let cmi: concrete_module_interface = ConcreteModuleInterface (mn, Docstring "", [], []) in
      let imports = resolve mn kind env body_imports in
      let decls = parse_defs mn cmi imports defs in
      (* Go through the declarations and make them public. *)
      let decls = List.map as_public decls in
      CombinedModule {
          name = mn;
          kind = kind;
          interface_docstring = Docstring "";
          interface_imports = empty_map mn;
          body_docstring = body_docstring;
          body_imports = imports;
          decls = decls;
    })
