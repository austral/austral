open Identifier
open Type
open MonoType
open MonoTypeBindings
open Tast
open Mtast
open Id
open DeclIdSet
open LexEnv
open EnvTypes
open EnvUtils
open Error

module Errors = struct
  let declaration_already_exists ~name ~module_name =
    austral_raise DeclarationError [
      Text "A declaration with the name ";
      Code (ident_string name);
      Text " already exists in the module ";
      Code (mod_name_string module_name);
      Text "."
    ]

  let unknown_module name =
    austral_raise DeclarationError [
      Text "Unknown module ";
      Code (mod_name_string name);
      Text "."
    ]
end

(** The file environment stores the contents of files for error reporting. *)
type file_env = file_rec list

(** The module environment contains module metadata. *)
type mod_env = mod_rec list

(** The instance method environment contains methods of typeclass instances. *)
type ins_meth_env = ins_meth_rec list

(** The declaration environment contains declarations. *)
type decl_env = decl list

(** The monomorph environment contains monomorphs. *)
type mono_env = monomorph list

type env = Env of {
      files: file_env;
      mods: mod_env;
      methods: ins_meth_env;
      decls: decl_env;
      monos: mono_env;
      exports: (decl_id * string) list;
      (** The list of exported functions. *)
    }

let empty_env: env =
  Env { files = []; mods = []; methods = []; decls = []; monos = []; exports = []; }

let get_module_by_id (env: env) (mod_id: mod_id): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { id; _ }) -> equal_mod_id mod_id id) mods

let get_module_by_name (env: env) (mod_name: module_name): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { name; _ }) -> equal_module_name mod_name name) mods

let ensure_no_mod_with_name (env: env) (name: module_name): unit =
  match get_module_by_name env name with
  | Some _ ->
     austral_raise DeclarationError [
         Text "A module with the name ";
         Code (mod_name_string name);
         Text " already exists.";
       ]
  | None ->
     ()

let module_name_from_id (env: env) (mod_id: mod_id): module_name =
  match get_module_by_id env mod_id with
  | Some (ModRec { name; _ }) ->
     name
  | _ ->
     internal_err "No module with this ID"

let add_file (env: env) (input: file_input): (env * file_id) =
  let { path; contents } = input in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_file_id () in
  let file = FileRec { id = id; path = path; contents = contents } in
  let env = Env { files = file :: files; mods; methods; decls; monos; exports } in
  (env, id)

let add_module (env: env) (input: mod_input): (env * mod_id) =
  let { name; interface_file; interface_docstring; body_file; body_docstring; kind; imported_instances; imports_from } = input in
  let _ = ensure_no_mod_with_name env name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_mod_id () in
  let md = ModRec {
               id;
               name;
               interface_file;
               interface_docstring;
               body_file;
               body_docstring;
               kind;
               imported_instances;
               imports_from;
             }
  in
  let env = Env { files; mods = md :: mods; methods; decls; monos; exports } in
  (env, id)

let get_decl_by_name (env: env) (name: sident): decl option =
  match get_module_by_name env (sident_module_name name) with
  | Some (ModRec { id=target_mod_id; _ }) ->
     let (Env { decls; _ }) = env in
     let pred (d: decl) =
       let mid = decl_mod_id d in
       if equal_mod_id target_mod_id mid then
         (* Module matches. *)
         (match (decl_name d) with
          | (Some dname) ->
             (* Does the name match? *)
             equal_identifier dname (sident_name name)
          | None ->
             false)
       else
         false
     in
     List.find_opt (fun d -> pred d) decls
  | None ->
     Errors.unknown_module (sident_module_name name)

let ensure_no_decl_with_name (env: env) (mod_id: mod_id) (name: identifier): unit =
  let mn: module_name = module_name_from_id env mod_id in
  let ident: sident = make_sident mn name in
  match get_decl_by_name env ident with
  | Some _ ->
     Errors.declaration_already_exists ~name ~module_name:mn
  | None ->
     ()

let make_const_decl (id: decl_id) (input: const_input): decl =
  let { mod_id; vis; name; ty; docstring } = input in
  Constant { id; mod_id; vis; name; ty; docstring }

let add_constant (env: env) (input: const_input): (env * decl_id) =
  (* Check: no other decl with this name. *)
  let _ = ensure_no_decl_with_name env input.mod_id input.name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_const_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_record_decl (id: decl_id) (input: record_input): decl =
  let { mod_id; vis; name; docstring; typarams; universe; slots } = input in
  Record { id; mod_id; vis; name; docstring; typarams; universe; slots }

let add_record (env: env) (input: record_input): (env * decl_id) =
  (* Check: no other decl with this name. *)
  let _ = ensure_no_decl_with_name env input.mod_id input.name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_record_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_union_decl (id: decl_id) (input: union_input): decl =
  let { mod_id; vis; name; docstring; typarams; universe } = input in
  Union { id; mod_id; vis; name; docstring; typarams; universe }

let add_union (env: env) (input: union_input): (env * decl_id) =
  (* Check: no other decl with this name. *)
  let _ = ensure_no_decl_with_name env input.mod_id input.name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_union_case_decl (id: decl_id) (input: union_case_input): decl =
  let { mod_id; vis; union_id; name; docstring; slots } = input in
  UnionCase { id; vis; mod_id; union_id; name; docstring; slots }

let add_union_case (env: env) (input: union_case_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_case_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_function_decl (id: decl_id) (input: function_input): decl =
  let { mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } = input in
  Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body }

let add_function (env: env) (input: function_input): (env * decl_id) =
  (* Check: no other decl with this name. *)
  let _ = ensure_no_decl_with_name env input.mod_id input.name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_function_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_type_class_decl (id: decl_id) (input: type_class_input): decl =
  let { mod_id; vis; name; docstring; param } = input in
  TypeClass { id; mod_id; vis; name; docstring; param }

let add_type_class (env: env) (input: type_class_input): (env * decl_id) =
  (* Check: no other decl with this name. *)
  let _ = ensure_no_decl_with_name env input.mod_id input.name in
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_type_class_method_decl (id: decl_id) (input: type_class_method_input): decl =
  let { mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt } = input in
  TypeClassMethod { id; mod_id; vis; typeclass_id; name; docstring; typarams; value_params; rt }

let add_type_class_method (env: env) (input: type_class_method_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_method_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_instance_decl (id: decl_id) (input: instance_input): decl =
  let { mod_id; vis; typeclass_id; docstring; typarams; argument } = input in
  Instance { id; mod_id; vis; typeclass_id; docstring; typarams; argument }

let add_instance (env: env) (input: instance_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_decl_id () in
  let decl = make_instance_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos; exports } in
  (env, id)

let make_ins_meth (id: ins_meth_id) (input: instance_method_input): ins_meth_rec =
  let { instance_id; method_id; docstring; name; typarams; value_params; rt; body } = input in
  InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body }

let add_instance_method (env: env) (input: instance_method_input): (env * ins_meth_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_ins_meth_id () in
  let meth = make_ins_meth id input in
  let env = Env { files; mods; methods = meth :: methods; decls; monos; exports } in
  (env, id)

let get_decl_by_id (env: env) (id: decl_id): decl option =
  let (Env { decls; _ }) = env in
  List.find_opt (fun d -> equal_decl_id id (decl_id d)) decls

let get_instance_method_by_id (env: env) (ins_meth_id: ins_meth_id): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id ins_meth_id) methods

let get_method_from_typeclass_id_and_name (env: env) (typeclass_id: decl_id) (name: identifier): decl option =
  let (Env { decls; _ }) = env in
  let pred (decl: decl): bool =
    match decl with
    | TypeClassMethod { typeclass_id=target_typeclass_id; name=target_name; _ } ->
       (equal_decl_id target_typeclass_id typeclass_id)
       && (equal_identifier target_name name)
    | _ ->
       false
  in
  List.find_opt pred decls

let module_instances (env: env) (id: mod_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | Instance { mod_id; _ } ->
       equal_mod_id mod_id id
    | _ -> false
  in
  List.filter pred decls

let module_public_instances (env: env) (id: mod_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | Instance { mod_id; vis; _ } ->
       (match vis with
        | VisPublic -> equal_mod_id mod_id id
        | VisPrivate -> false)
    | _ -> false
  in
  List.filter pred decls

let get_union_cases (env: env) (id: decl_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | UnionCase { union_id; _ } ->
       equal_decl_id union_id id
    | _ -> false
  in
  List.filter pred decls

let get_callable (env: env) (importing_module_name: module_name) (name: sident): callable option =
  let _ = importing_module_name in
  match get_decl_by_name env name with
  | Some decl ->
     (match decl with
      | Record { id; typarams; universe; slots; _ } ->
         Some (RecordConstructor (id, typarams, universe, slots))
      | UnionCase { name; union_id; slots; _ } ->
         let (typarams, universe) =
           (match get_decl_by_id env union_id with
            | Some (Union { typarams; universe; _}) -> (typarams, universe)
            | _ -> err "No union with ID")
         in
         Some (
             UnionConstructor {
                 union_id = union_id;
                 type_params = typarams;
                 universe = universe;
                 case = TypedCase (name, slots)
           })
      | Function { id; typarams; value_params; rt; _ } ->
         Some (FunctionCallable (id, typarams, value_params, rt))
      | TypeClassMethod { typeclass_id; value_params; rt; _ } ->
         Some (
             MethodCallable {
                 typeclass_id = typeclass_id;
                 value_parameters = value_params;
                 return_type = rt;
               }
           )
      | _ ->
         None)
  | None ->
     None

let get_variable (env: env) (lexenv: lexenv) (name: qident): (ty * var_source) option =
  match get_var lexenv (original_name name) with
  | Some (ty, src) ->
     Some (ty, src)
  | None ->
     (match get_decl_by_name env (qident_to_sident name) with
      | Some decl ->
         (match decl with
          | Constant { ty; _ } ->
             Some (ty, VarConstant)
          | _ ->
             None)
      | None ->
         None)

let visible_instances (env: env) (id: mod_id): decl list =
  let instances_defined_in_module: decl list =
    let (Env { decls; _ }) = env
    and pred = function
      | Instance { mod_id; _ } ->
         equal_mod_id id mod_id
      | _ -> false
    in
    List.filter pred decls
  and instances_imported_by_module: decl list =
    match get_module_by_id env id with
    | Some (ModRec { imported_instances; _ }) ->
       List.map (fun id -> Option.get (get_decl_by_id env id)) (List.of_seq (DeclIdSet.to_seq imported_instances))
    | None ->
       internal_err "No module with ID."
  in
  instances_defined_in_module @ instances_imported_by_module

let rec store_function_body (env: env) (fn_id: decl_id) (body: tstmt): env =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let decl: decl = (match get_decl_by_id env fn_id with
                    | Some d ->
                       d
                    | _ ->
                       internal_err ("No function with this ID: " ^ (show_decl_id fn_id)))
  in
  let new_decl: decl = replace_function_body decl body in
  let decls_without_existing_one: decl list = List.filter (fun (d: decl) -> not (equal_decl_id (decl_id d) fn_id)) decls in
  let env = Env { files; mods; methods; decls = new_decl :: decls_without_existing_one; monos; exports } in
  env

and replace_function_body (decl: decl) (new_body: tstmt): decl =
  match decl with
  | Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } ->
     (match body with
      | Some _ ->
         internal_err ("function `" ^ (ident_string name) ^ "` already has a body")
      | None ->
         Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body=(Some new_body) })
  | _ ->
     internal_err "not a function."

let rec store_method_body (env: env) (ins_meth_id: ins_meth_id) (body: tstmt): env =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let meth: ins_meth_rec = (match get_instance_method_by_id env ins_meth_id with
                              | Some m ->
                                 m
                              | _ ->
                                 internal_err ("No instance method with this ID " ^ (show_ins_meth_id ins_meth_id)))
  in
  let new_method: ins_meth_rec = replace_method_body meth body in
  let methods_without_existing_one: ins_meth_rec list = List.filter (fun (InsMethRec { id; _ }) -> not (equal_ins_meth_id id ins_meth_id)) methods in
  let env = Env { files; mods; methods = new_method :: methods_without_existing_one; decls; monos; exports } in
  env

and replace_method_body (meth: ins_meth_rec) (new_body: tstmt): ins_meth_rec =
  let (InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body }) = meth in
  (match body with
   | Some _ ->
      internal_err ("function `" ^ (ident_string name) ^ "` already has a body")
   | None ->
      InsMethRec { id; instance_id; method_id; docstring; name; typarams; value_params; rt; body=(Some new_body) })

let get_instance_method_from_instance_id_and_method_name (env: env) (instance_id: decl_id) (name: identifier): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  let pred (InsMethRec { instance_id=target_instance_id; name=target_name; _ }): bool =
    (equal_decl_id instance_id target_instance_id)
    && (equal_identifier name target_name)
  in
  List.find_opt pred methods

let add_record_monomorph (env: env) (type_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_mono_id () in
  let mono = MonoRecordDefinition { id; type_id; tyargs; slots = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos; exports } in
  (env, id)

let add_union_monomorph (env: env) (type_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_mono_id () in
  let mono = MonoUnionDefinition { id; type_id; tyargs; cases = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos; exports } in
  (env, id)

let add_function_monomorph (env: env) (function_id: decl_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_mono_id () in
  let mono = MonoFunction { id; function_id; tyargs; body = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos; exports } in
  (env, id)

let add_instance_method_monomorph (env: env) (method_id: ins_meth_id) (tyargs: mono_type_bindings): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let id = fresh_mono_id () in
  let mono = MonoInstanceMethod { id; method_id; tyargs; body = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos; exports } in
  (env, id)

let monomorph_id (mono: monomorph): mono_id =
  match mono with
  | MonoRecordDefinition { id; _ } -> id
  | MonoUnionDefinition { id; _ } -> id
  | MonoFunction { id; _ } -> id
  | MonoInstanceMethod { id; _ } -> id

let get_type_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoRecordDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (equal_mono_bindings tyargs args)
    | MonoUnionDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_record_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match get_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_record_monomorph env decl_id args

let add_or_get_union_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match get_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_union_monomorph env decl_id args

let get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoFunction { function_id; tyargs; _ } ->
      (equal_decl_id function_id decl_id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_type_bindings): (env * mono_id) =
  match get_function_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_function_monomorph env decl_id args

let get_instance_method_monomorph (env: env) (id: ins_meth_id) (args: mono_type_bindings): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoInstanceMethod { method_id; tyargs; _ } ->
      (equal_ins_meth_id method_id id)
      && (equal_mono_bindings tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_instance_method_monomorph (env: env) (method_id: ins_meth_id) (args: mono_type_bindings): (env * mono_id) =
  match get_instance_method_monomorph env method_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_instance_method_monomorph env method_id args

let is_instantiated (mono: monomorph): bool =
  match mono with
  | MonoRecordDefinition { slots; _ } ->
    Option.is_some slots
  | MonoUnionDefinition { cases; _ } ->
    Option.is_some cases
  | MonoFunction { body; _ } ->
    Option.is_some body
  | MonoInstanceMethod { body; _ } ->
    Option.is_some body

let get_uninstantiated_monomorphs (env: env): monomorph list =
  let (Env { monos; _ }) = env in
  let pred (m: monomorph) =
    not (is_instantiated m)
  in
  List.rev (List.filter pred monos)

(** Find a monomorph by ID and return it, removing it from the
   environment. Errors if it does not exist. *)
let pop_monomorph (env: env) (id: mono_id): (env * monomorph) =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let pred (mono: monomorph): bool =
    let target_id: mono_id = monomorph_id mono in
    equal_mono_id target_id id
  in
  match List.find_opt pred monos with
  | Some mono ->
    let other_monos = List.filter (fun m -> not (pred m)) monos in
    let env = Env { files; mods; methods; decls; monos = other_monos; exports } in
    (env, mono)
  | None ->
    internal_err ("monomorph with this ID does not exist: " ^ (show_mono_id id))

let store_record_monomorph_definition (env: env) (id: mono_id) (new_slots: mono_slot list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoRecordDefinition { id; type_id; tyargs; slots; } ->
    let _ = slots in
    let new_mono = MonoRecordDefinition { id; type_id; tyargs; slots = Some new_slots; } in
    let (Env { files; mods; methods; decls; monos; exports }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos; exports } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph record definition with ID: " ^ (show_mono_id id))

let store_union_monomorph_definition (env: env) (id: mono_id) (new_cases: mono_case list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoUnionDefinition { id; type_id; tyargs; cases; } ->
    let _ = cases in
    let new_mono = MonoUnionDefinition { id; type_id; tyargs; cases = Some new_cases; } in
    let (Env { files; mods; methods; decls; monos; exports }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos; exports } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph union definition with ID: " ^ (show_mono_id id))

let store_function_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoFunction { id; function_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoFunction { id; function_id; tyargs; body = Some new_body; } in
    let (Env { files; mods; methods; decls; monos; exports }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos; exports } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph function definition with ID: " ^ (show_mono_id id))

let store_instance_method_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoInstanceMethod { id; method_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoInstanceMethod { id; method_id; tyargs; body = Some new_body; } in
    let (Env { files; mods; methods; decls; monos; exports }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos; exports } in
    env
  | _ ->
    internal_err ("Couldn't find monomorph instance method definition with ID: " ^ (show_mono_id id))

let get_instance_method (env: env) (target_id: ins_meth_id): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id target_id) methods

let get_monomorph (env: env) (id: mono_id): monomorph option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    equal_mono_id id (monomorph_id mono)
  in
  List.find_opt pred monos

let add_exported_function (env: env) (id: decl_id) (export_name: string): env =
  let (Env { files; mods; methods; decls; monos; exports }) = env in
  let env = Env { files; mods; methods; decls; monos; exports = (id, export_name) :: exports } in
  env

let get_export_functions (env: env): (decl_id * string) list =
  let (Env { exports; _ }) = env in
  exports
