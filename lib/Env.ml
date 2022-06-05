open Identifier
open Common
open Type
open TypeParameters
open TypeSignature
open MonoType
open Tast
open Mtast
open Id
open LexEnv
open TypeClasses
open Error

type file_rec = FileRec of { id: file_id; path: string; contents: string }

type mod_rec = ModRec of {
      id: mod_id;
      name: module_name;
      interface_file: file_id;
      interface_docstring: docstring;
      body_file: file_id;
      body_docstring: docstring;
      kind: module_kind
    }

type decl =
  | Constant of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      name: identifier;
      ty: ty;
      docstring: docstring;
    }
  | TypeAlias of {
      id: decl_id;
      mod_id: mod_id;
      vis: type_vis;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      universe: universe;
      def: ty;
    }
  | Record of {
      id: decl_id;
      mod_id: mod_id;
      vis: type_vis;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      universe: universe;
      slots: typed_slot list;
    }
  | Union of {
      id: decl_id;
      mod_id: mod_id;
      vis: type_vis;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      universe: universe;
    }
  | UnionCase of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      union_id: decl_id;
      name: identifier;
      docstring: docstring;
      slots: typed_slot list;
    }
  | Function of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      value_params: value_parameter list;
      rt: ty;
      external_name: string option;
      (** If this function is foreign, this is the name of the underlying function
          that will be called. *)
      body: tstmt option;
    }
  | TypeClass of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      name: identifier;
      docstring: docstring;
      param: type_parameter;
    }
  | TypeClassMethod of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      typeclass_id: decl_id;
      name: identifier;
      docstring: docstring;
      value_params: value_parameter list;
      rt: ty;
    }
  | Instance of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      typeclass_id: decl_id;
      docstring: docstring;
      typarams: typarams;
      argument: instance_argument;
    }

type ins_meth_rec = InsMethRec of {
      id: ins_meth_id;
      instance_id: decl_id;
      method_id: decl_id;
      docstring: docstring;
      name: identifier;
      value_params: value_parameter list;
      rt: ty;
      body: tstmt option;
    }

type monomorph =
  | MonoTypeAliasDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      def: mono_ty option;
    }
  | MonoRecordDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      slots: (mono_slot list) option;
    }
  | MonoUnionDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      cases: (mono_case list) option;
    }
  | MonoFunction of {
      id: mono_id;
      function_id: decl_id;
      tyargs: mono_ty list;
      body: mstmt option;
    }
  | MonoInstanceMethod of {
      id: mono_id;
      method_id: ins_meth_id;
      tyargs: mono_ty list;
      body: mstmt option;
    }

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
    }

let empty_env: env =
  Env { files = []; mods = []; methods = []; decls = []; monos = []; }

type file_input = { path: string; contents: string }

let add_file (env: env) (input: file_input): (env * file_id) =
  let { path; contents } = input in
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_file_id () in
  let file = FileRec { id = id; path = path; contents = contents } in
  let env = Env { files = file :: files; mods; methods; decls; monos} in
  (env, id)

type mod_input = {
    name: module_name;
    interface_file: file_id;
    interface_docstring: docstring;
    body_file: file_id;
    body_docstring: docstring;
    kind: module_kind
  }

let add_module (env: env) (input: mod_input): (env * mod_id) =
  let { name; interface_file; interface_docstring; body_file; body_docstring; kind } = input in
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mod_id () in
  let md = ModRec {
               id;
               name;
               interface_file;
               interface_docstring;
               body_file;
               body_docstring;
               kind;
             }
  in
  let env = Env { files; mods = md :: mods; methods; decls; monos } in
  (env, id)

let get_module_by_id (env: env) (mod_id: mod_id): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { id; _ }) -> equal_mod_id mod_id id) mods

let get_module_by_name (env: env) (mod_name: module_name): mod_rec option =
  let (Env { mods; _ }) = env in
  List.find_opt (fun (ModRec { name; _ }) -> equal_module_name mod_name name) mods

type const_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    ty: ty;
    docstring: docstring;
  }

let make_const_decl (id: decl_id) (input: const_input): decl =
  let { mod_id; vis; name; ty; docstring } = input in
  Constant { id; mod_id; vis; name; ty; docstring }

let add_constant (env: env) (input: const_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_const_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type type_alias_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    def: ty;
  }

let make_type_alias_decl (id: decl_id) (input: type_alias_input): decl =
  let { mod_id; vis; name; docstring; typarams; universe; def } = input in
  TypeAlias { id; mod_id; vis; name; docstring; typarams; universe; def }

let add_type_alias (env: env) (input: type_alias_input): (env * decl_id) =
  let id = fresh_decl_id () in
  let decl = make_type_alias_decl id input in
  let (Env { files; mods; methods; decls; monos }) = env in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type record_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    slots: typed_slot list;
  }

let make_record_decl (id: decl_id) (input: record_input): decl =
  let { mod_id; vis; name; docstring; typarams; universe; slots } = input in
  Record { id; mod_id; vis; name; docstring; typarams; universe; slots }

let add_record (env: env) (input: record_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_record_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type union_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

let make_union_decl (id: decl_id) (input: union_input): decl =
  let { mod_id; vis; name; docstring; typarams; universe } = input in
  Union { id; mod_id; vis; name; docstring; typarams; universe }

let add_union (env: env) (input: union_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type union_case_input = {
    mod_id: mod_id;
    vis: vis;
    union_id: decl_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

let make_union_case_decl (id: decl_id) (input: union_case_input): decl =
  let { mod_id; vis; union_id; name; docstring; slots } = input in
  UnionCase { id; vis; mod_id; union_id; name; docstring; slots }

let add_union_case (env: env) (input: union_case_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_case_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type function_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    value_params: value_parameter list;
    rt: ty;
    external_name: string option;
    body: tstmt option;
  }

let make_function_decl (id: decl_id) (input: function_input): decl =
  let { mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } = input in
  Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body }

let add_function (env: env) (input: function_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_function_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type type_class_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

let make_type_class_decl (id: decl_id) (input: type_class_input): decl =
  let { mod_id; vis; name; docstring; param } = input in
  TypeClass { id; mod_id; vis; name; docstring; param }

let add_type_class (env: env) (input: type_class_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type type_class_method_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    name: identifier;
    docstring: docstring;
    value_params: value_parameter list;
    rt: ty;
  }

let make_type_class_method_decl (id: decl_id) (input: type_class_method_input): decl =
  let { mod_id; vis; typeclass_id; name; docstring; value_params; rt } = input in
  TypeClassMethod { id; mod_id; vis; typeclass_id; name; docstring; value_params; rt }

let add_type_class_method (env: env) (input: type_class_method_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_method_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type instance_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    docstring: docstring;
    typarams: typarams;
    argument: instance_argument;
  }

let make_instance_decl (id: decl_id) (input: instance_input): decl =
  let { mod_id; vis; typeclass_id; docstring; typarams; argument } = input in
  Instance { id; mod_id; vis; typeclass_id; docstring; typarams; argument }

let add_instance (env: env) (input: instance_input): (env * decl_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_decl_id () in
  let decl = make_instance_decl id input in
  let env = Env { files; mods; methods; decls = decl :: decls; monos } in
  (env, id)

type instance_method_input = {
    instance_id: decl_id;
    method_id: decl_id;
    docstring: docstring;
    name: identifier;
    value_params: value_parameter list;
    rt: ty;
    body: tstmt option;
  }

let make_ins_meth (id: ins_meth_id) (input: instance_method_input): ins_meth_rec =
  let { instance_id; method_id; docstring; name; value_params; rt; body } = input in
  InsMethRec { id; instance_id; method_id; docstring; name; value_params; rt; body }

let add_instance_method (env: env) (input: instance_method_input): (env * ins_meth_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_ins_meth_id () in
  let meth = make_ins_meth id input in
  let env = Env { files; mods; methods = meth :: methods; decls; monos } in
  (env, id)

let decl_id (decl: decl): decl_id =
  match decl with
  | Constant { id; _ } -> id
  | TypeAlias { id; _ } -> id
  | Record { id; _ } -> id
  | Union { id; _ } -> id
  | UnionCase { id; _ } -> id
  | Function { id; _ } -> id
  | TypeClass { id; _ } -> id
  | TypeClassMethod { id; _ } -> id
  | Instance { id; _ } -> id

let decl_mod_id (decl: decl): mod_id =
  match decl with
  | Constant { mod_id; _ } -> mod_id
  | TypeAlias { mod_id; _ } -> mod_id
  | Record { mod_id; _ } -> mod_id
  | Union { mod_id; _ } -> mod_id
  | UnionCase { mod_id; _ } -> mod_id
  | Function { mod_id; _ } -> mod_id
  | TypeClass { mod_id; _ } -> mod_id
  | TypeClassMethod { mod_id; _ } -> mod_id
  | Instance { mod_id; _ } -> mod_id

let decl_name (decl: decl): identifier option =
  match decl with
  | Constant { name; _ } -> Some name
  | TypeAlias { name; _ } -> Some name
  | Record { name; _ } -> Some name
  | Union { name; _ } -> Some name
  | UnionCase { name; _ } -> Some name
  | Function { name; _ } -> Some name
  | TypeClass { name; _ } -> Some name
  | TypeClassMethod { name; _ } -> Some name
  | Instance _ -> None

let get_decl_by_id (env: env) (id: decl_id): decl option =
  let (Env { decls; _ }) = env in
  List.find_opt (fun d -> equal_decl_id id (decl_id d)) decls

let get_instance_method_by_id (env: env) (ins_meth_id: ins_meth_id): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id ins_meth_id) methods

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
     err "No such module."

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

let is_importable (decl: decl): bool =
  let importable_vis = function
    | VisPublic -> true
    | VisPrivate -> false
  and importable_type = function
    | TypeVisPublic -> true
    | TypeVisOpaque -> true
    | TypeVisPrivate -> false
  in
  match decl with
  | Constant { vis; _ } -> importable_vis vis
  | TypeAlias { vis; _ } -> importable_type vis
  | Record { vis; _ } -> importable_type vis
  | Union { vis; _ } -> importable_type vis
  | UnionCase { vis; _ } -> importable_vis vis
  | Function { vis; _ } -> importable_vis vis
  | TypeClass { vis; _ } -> importable_vis vis
  | TypeClassMethod { vis; _ } -> importable_vis vis
  | Instance _ -> true

let module_instances (env: env) (id: mod_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | Instance { mod_id; _ } ->
       equal_mod_id mod_id id
    | _ -> false
  in
  List.filter pred decls

let module_name_from_id (env: env) (mod_id: mod_id): module_name =
  match get_module_by_id env mod_id with
  | Some (ModRec { name; _ }) ->
     name
  | _ ->
     err "Internal error: no module with ID"

let get_union_cases (env: env) (id: decl_id): decl list =
  let (Env { decls; _ }) = env
  and pred = function
    | UnionCase { union_id; _ } ->
       equal_decl_id union_id id
    | _ -> false
  in
  List.filter pred decls

let union_case_to_typed_case (decl: decl): typed_case =
  match decl with
  | UnionCase { name; slots; _ } ->
     TypedCase (name, slots)
  | _ ->
     err "Internal: not a union case"

type callable =
  | FunctionCallable of decl_id * typarams * value_parameter list * ty
  | TypeAliasCallable of decl_id * typarams * universe * ty
  | RecordConstructor of decl_id * typarams * universe * typed_slot list
  | UnionConstructor of {
      union_id: decl_id;
      type_params: typarams;
      universe: universe;
      case: typed_case;
    }
  | MethodCallable of {
      typeclass_id: decl_id;
      value_parameters: value_parameter list;
      return_type: ty
    }

let get_callable (env: env) (importing_module_name: module_name) (name: sident): callable option =
  let _ = importing_module_name in
  match get_decl_by_name env name with
  | Some decl ->
     (match decl with
      | TypeAlias { id; typarams; universe; def; _ } ->
         Some (TypeAliasCallable (id, typarams, universe, def))
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

let visible_instances (env: env): decl list =
  (* TODO: filter correctly using the source module ID *)
  let (Env { decls; _ }) = env
  and pred = function
      | Instance _ -> true
      | _ -> false
  in
  List.filter pred decls

let rec store_function_body (env: env) (fn_id: decl_id) (body: tstmt): env =
  let (Env { files; mods; methods; decls; monos }) = env in
  let decl: decl = (match get_decl_by_id env fn_id with
                    | Some d ->
                       d
                    | _ ->
                       err "Internal: No function with this ID.")
  in
  let new_decl: decl = replace_function_body decl body in
  let decls_without_existing_one: decl list = List.filter (fun (d: decl) -> not (equal_decl_id (decl_id d) fn_id)) decls in
  let env = Env { files; mods; methods; decls = new_decl :: decls_without_existing_one; monos } in
  env

and replace_function_body (decl: decl) (new_body: tstmt): decl =
  match decl with
  | Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body } ->
     (match body with
      | Some _ ->
         err "Internal: function already has a body"
      | None ->
         Function { id; mod_id; vis; name; docstring; typarams; value_params; rt; external_name; body=(Some new_body) })
  | _ ->
     err "Internal: not a function."

let rec store_method_body (env: env) (ins_meth_id: ins_meth_id) (body: tstmt): env =
  let (Env { files; mods; methods; decls; monos }) = env in
  let meth: ins_meth_rec = (match get_instance_method_by_id env ins_meth_id with
                              | Some m ->
                                 m
                              | _ ->
                                 err "Internal: No instance method with this ID.")
  in
  let new_method: ins_meth_rec = replace_method_body meth body in
  let methods_without_existing_one: ins_meth_rec list = List.filter (fun (InsMethRec { id; _ }) -> not (equal_ins_meth_id id ins_meth_id)) methods in
  let env = Env { files; mods; methods = new_method :: methods_without_existing_one; decls; monos } in
  env

and replace_method_body (meth: ins_meth_rec) (new_body: tstmt): ins_meth_rec =
  let (InsMethRec { id; instance_id; method_id; docstring; name; value_params; rt; body }) = meth in
  (match body with
   | Some _ ->
      err "Internal: function already has a body"
   | None ->
      InsMethRec { id; instance_id; method_id; docstring; name; value_params; rt; body=(Some new_body) })

let get_instance_method_from_instance_id_and_method_name (env: env) (instance_id: decl_id) (name: identifier): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  let pred (InsMethRec { instance_id=target_instance_id; name=target_name; _ }): bool =
    (equal_decl_id instance_id target_instance_id)
    && (equal_identifier name target_name)
  in
  List.find_opt pred methods

let add_type_alias_monomorph (env: env) (type_id: decl_id) (tyargs: mono_ty list): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mono_id () in
  let mono = MonoTypeAliasDefinition { id; type_id; tyargs; def = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos } in
  (env, id)

let add_record_monomorph (env: env) (type_id: decl_id) (tyargs: mono_ty list): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mono_id () in
  let mono = MonoRecordDefinition { id; type_id; tyargs; slots = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos } in
  (env, id)

let add_union_monomorph (env: env) (type_id: decl_id) (tyargs: mono_ty list): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mono_id () in
  let mono = MonoUnionDefinition { id; type_id; tyargs; cases = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos } in
  (env, id)

let add_function_monomorph (env: env) (function_id: decl_id) (tyargs: mono_ty list): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mono_id () in
  let mono = MonoFunction { id; function_id; tyargs; body = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos } in
  (env, id)

let add_instance_method_monomorph (env: env) (method_id: ins_meth_id) (tyargs: mono_ty list): (env * mono_id) =
  let (Env { files; mods; methods; decls; monos }) = env in
  let id = fresh_mono_id () in
  let mono = MonoInstanceMethod { id; method_id; tyargs; body = None } in
  let env = Env { files; mods; methods; decls; monos = mono :: monos } in
  (env, id)

let monomorph_id (mono: monomorph): mono_id =
  match mono with
  | MonoTypeAliasDefinition { id; _ } -> id
  | MonoRecordDefinition { id; _ } -> id
  | MonoUnionDefinition { id; _ } -> id
  | MonoFunction { id; _ } -> id
  | MonoInstanceMethod { id; _ } -> id

let get_type_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoTypeAliasDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (List.equal equal_mono_ty tyargs args)
    | MonoRecordDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (List.equal equal_mono_ty tyargs args)
    | MonoUnionDefinition { type_id; tyargs; _ } ->
      (equal_decl_id type_id decl_id)
      && (List.equal equal_mono_ty tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_type_alias_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): (env * mono_id) =
  (* TODO: Possibly type confusion vulnerability, since `get_type_monomorph`
     doesn't care whether the mono is a type alias or a record or a union? *)
  match get_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_type_alias_monomorph env decl_id args

let add_or_get_record_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): (env * mono_id) =
  match get_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_record_monomorph env decl_id args

let add_or_get_union_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): (env * mono_id) =
  match get_type_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_union_monomorph env decl_id args

let get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoFunction { function_id; tyargs; _ } ->
      (equal_decl_id function_id decl_id)
      && (List.equal equal_mono_ty tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_function_monomorph (env: env) (decl_id: decl_id) (args: mono_ty list): (env * mono_id) =
  match get_function_monomorph env decl_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_function_monomorph env decl_id args

let get_instance_method_monomorph (env: env) (id: ins_meth_id) (args: mono_ty list): mono_id option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    match mono with
    | MonoInstanceMethod { method_id; tyargs; _ } ->
      (equal_ins_meth_id method_id id)
      && (List.equal equal_mono_ty tyargs args)
    | _ ->
      false
  in
  match List.find_opt pred monos with
  | Some m -> Some (monomorph_id m)
  | None -> None

let add_or_get_instance_method_monomorph (env: env) (method_id: ins_meth_id) (args: mono_ty list): (env * mono_id) =
  match get_instance_method_monomorph env method_id args with
  | Some mono_id -> (env, mono_id)
  | None ->
    add_instance_method_monomorph env method_id args

let is_instantiated (mono: monomorph): bool =
  match mono with
  | MonoTypeAliasDefinition { def; _ } ->
    Option.is_some def
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
  let (Env { files; mods; methods; decls; monos }) = env in
  let pred (mono: monomorph): bool =
    let target_id: mono_id = monomorph_id mono in
    equal_mono_id target_id id
  in
  match List.find_opt pred monos with
  | Some mono ->
    let other_monos = List.filter (fun m -> not (pred m)) monos in
    let env = Env { files; mods; methods; decls; monos = other_monos } in
    (env, mono)
  | None ->
    err "internal: monomorph with this ID does not exist"

let store_type_alias_monomorph_definition (env: env) (id: mono_id) (ty: mono_ty): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoTypeAliasDefinition { id; type_id; tyargs; def; } ->
    let _ = def in
    let new_mono = MonoTypeAliasDefinition { id; type_id; tyargs; def = Some ty; } in
    let (Env { files; mods; methods; decls; monos }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos } in
    env
  | _ ->
    err "internal"

let store_record_monomorph_definition (env: env) (id: mono_id) (new_slots: mono_slot list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoRecordDefinition { id; type_id; tyargs; slots; } ->
    let _ = slots in
    let new_mono = MonoRecordDefinition { id; type_id; tyargs; slots = Some new_slots; } in
    let (Env { files; mods; methods; decls; monos }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos } in
    env
  | _ ->
    err "internal"

let store_union_monomorph_definition (env: env) (id: mono_id) (new_cases: mono_case list): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoUnionDefinition { id; type_id; tyargs; cases; } ->
    let _ = cases in
    let new_mono = MonoUnionDefinition { id; type_id; tyargs; cases = Some new_cases; } in
    let (Env { files; mods; methods; decls; monos }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos } in
    env
  | _ ->
    err "internal"

let store_function_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoFunction { id; function_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoFunction { id; function_id; tyargs; body = Some new_body; } in
    let (Env { files; mods; methods; decls; monos }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos } in
    env
  | _ ->
    err "internal"

let store_instance_method_monomorph_definition (env: env) (id: mono_id) (new_body: mstmt): env =
  let (env, mono) = pop_monomorph env id in
  match mono with
  | MonoInstanceMethod { id; method_id; tyargs; body; } ->
    let _ = body in
    let new_mono = MonoInstanceMethod { id; method_id; tyargs; body = Some new_body; } in
    let (Env { files; mods; methods; decls; monos }) = env in
    let env = Env { files; mods; methods; decls; monos = new_mono :: monos } in
    env
  | _ ->
    err "internal"

let get_instance_method (env: env) (target_id: ins_meth_id): ins_meth_rec option =
  let (Env { methods; _ }) = env in
  List.find_opt (fun (InsMethRec { id; _ }) -> equal_ins_meth_id id target_id) methods

let get_monomorph (env: env) (id: mono_id): monomorph option =
  let (Env { monos; _ }) = env in
  let pred (mono: monomorph): bool =
    equal_mono_id id (monomorph_id mono)
  in
  List.find_opt pred monos

let decl_type_signature (decl: decl): type_signature option =
  match decl with
  | Constant _ ->
     None
  | TypeAlias { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | Record { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | Union { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | UnionCase _ ->
     None
  | Function _ ->
     None
  | TypeClass _ ->
     None
  | TypeClassMethod _ ->
     None
  | Instance _ ->
     None

let get_type_signature_by_name (env: env) (name: sident): type_signature =
  match get_decl_by_name env name with
  | Some decl ->
     (match decl_type_signature decl with
      | Some s -> s
      | None ->
         err "Not a type")
  | None ->
     err "Internal: no decl"
