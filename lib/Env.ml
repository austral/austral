open Identifier
open Common
open Type
open Tast
open Error

type typarams = type_parameter list

type file_id = FileId of int
[@@deriving eq]

type mod_id = ModId of int
[@@deriving eq]

type decl_id = DeclId of int
[@@deriving eq]

type ins_meth_id = InsMethId of int
[@@deriving eq]

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
      typeclass_id: decl_id;
      docstring: docstring;
      typarams: typarams;
      argument: ty;
    }

(** Represents an instance method. *)
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

(** The file environment stores the contents of files for error reporting. *)
type file_env = file_rec list

(** The module environment contains module metadata. *)
type mod_env = mod_rec list

(** The instance method environment contains methods of typeclass instances. *)
type ins_meth_env = ins_meth_rec list

(** The declaration environment contains declarations. *)
type decl_env = decl list

type env = Env of {
      files: file_env;
      mods: mod_env;
      methods: ins_meth_env;
      decls: decl_env;
    }

(* ID utilities *)

let file_counter: int ref = ref 1

let fresh_file_id _: file_id =
  let id = !file_counter in
  file_counter := id + 1;
  FileId id

let mod_counter: int ref = ref 1

let fresh_mod_id _: mod_id =
  let id = !mod_counter in
  mod_counter := id + 1;
  ModId id

let decl_counter: int ref = ref 1

let fresh_decl_id _: decl_id =
  let id = !decl_counter in
  decl_counter := id + 1;
  DeclId id

let ins_meth_counter: int ref = ref 1

let fresh_ins_meth_id _: ins_meth_id =
  let id = !ins_meth_counter in
  ins_meth_counter := id + 1;
  InsMethId id

let empty_env: env =
  Env { files = []; mods = []; methods = []; decls = [] }

type file_input = { path: string; contents: string }

let add_file (env: env) (input: file_input): (env * file_id) =
  let { path; contents } = input in
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_file_id () in
  let file = FileRec { id = id; path = path; contents = contents } in
  let env = Env { files = file :: files; mods = mods; methods = methods; decls = decls } in
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
  let (Env { files; mods; methods; decls }) = env in
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
  let env = Env { files = files; mods = md :: mods; methods = methods; decls = decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_const_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_record_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_union_case_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_function_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_type_class_method_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
  (env, id)

type instance_input = {
    mod_id: mod_id;
    typeclass_id: decl_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

let make_instance_decl (id: decl_id) (input: instance_input): decl =
  let { mod_id; typeclass_id; docstring; typarams; argument } = input in
  Instance { id; mod_id; typeclass_id; docstring; typarams; argument }

let add_instance (env: env) (input: instance_input): (env * decl_id) =
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_decl_id () in
  let decl = make_instance_decl id input in
  let env = Env { files = files; mods = mods; methods = methods; decls = decl :: decls } in
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
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_ins_meth_id () in
  let meth = make_ins_meth id input in
  let env = Env { files = files; mods = mods; methods = meth :: methods; decls = decls } in
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
