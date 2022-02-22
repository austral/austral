open Identifier
open Common
open Type
open Tast

(** A set of type parameters. *)
type typarams = type_parameter list

(** The type of file IDs. *)
type file_id
[@@deriving eq]

(** The type of module IDs. *)
type mod_id = ModId of int
[@@deriving eq]

(** The type of declaration IDs. *)
type decl_id = DeclId of int
[@@deriving eq]

(** The type of instance method IDs. *)
type ins_meth_id = InsMethId of int
[@@deriving eq]

(** The global compiler environment. *)
type env

(** A file record contains a file's path and contents. *)
type file_rec = FileRec of { id: file_id; path: string; contents: string }

(** The empty compiler. *)
val empty_env : env

(** Type of the input to the {!add_file} function. *)
type file_input = { path: string; contents: string }

(** Add a file's path and contents to the environment. *)
val add_file : env -> file_input -> (env * file_id)

(** Type of the input to the {!add_module} function. *)
type mod_input = {
    name: module_name;
    docstring: docstring;
    interface_file: file_id;
    body_file: file_id;
    is_unsafe: bool;
  }

(** Add a module's information to the environment. *)
val add_module : env -> mod_input -> (env * mod_id)

(** A module record contains a module's name, docstring, and pointers to the
   interface and body files. *)
type mod_rec = ModRec of {
      id: mod_id;
      name: module_name;
      docstring: docstring;
      interface_file: file_id;
      body_file: file_id;
      is_unsafe: bool
    }

(** Retrieve a module by its module ID. *)
val get_module_by_id : env -> mod_id -> mod_rec option

(** Retrieve a module by its name. *)
val get_module_by_name : env -> module_name -> mod_rec option

(** Input to the {!add_const} function. *)
type const_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
  }

val add_constant : env -> const_input -> (env * decl_id)

(** Input to the {!add_type_alias} function. *)
type type_alias_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    def: ty;
  }

val add_type_alias : env -> type_alias_input -> (env * decl_id)

(** Input to the {!add_record} function. *)
type record_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    slots: typed_slot list;
  }

val add_record : env -> record_input -> (env * decl_id)

(** Input to the {!add_union} function. *)
type union_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

val add_union : env -> union_input -> (env * decl_id)

(** Input to the {!add_union_case} function. *)
type union_case_input = {
    mod_id: mod_id;
    union_id: decl_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

val add_union_case : env -> union_case_input -> (env * decl_id)

(** Input to the {!add_function} function. *)
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

val add_function : env -> function_input -> (env * decl_id)

(** Input to the {!add_type_class} function. *)
type type_class_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

val add_type_class : env -> type_class_input -> (env * decl_id)

(** Input to the {!add_type_class_method} function. *)
type type_class_method_input = {
    mod_id: mod_id;
    typeclass_id: decl_id;
    name: identifier;
    docstring: docstring;
    value_params: value_parameter list;
    rt: ty;
  }

val add_type_class_method : env -> type_class_method_input -> (env * decl_id)

(** Input to the {!add_instance} function. *)
type instance_input = {
    mod_id: mod_id;
    typeclass_id: decl_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

val add_instance : env -> instance_input -> (env * decl_id)

(** Input to the {!add_instance_method} function. *)
type instance_method_input = {
    instance_id: decl_id;
    docstring: docstring;
    value_params: value_parameter list;
    rt: ty;
    body: tstmt option;
  }

val add_instance_method : env -> instance_method_input -> (env * ins_meth_id)

(** A declaration record represents a declaration.

   The definition of declaration here is slightly different than that of the
   spec: we include union cases and typeclass methods as "declarations". The
   reason being that we want union cases and typeclass methods to share a
   namespace with real declarations, and this is an easy way to make that
   happen.

*)
type decl =
  | Constant of {
      id: decl_id;
      mod_id: mod_id;
      vis: vis;
      name: identifier;
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

(** Retrieve a declaration by ID, returning {!None} if it doesn't exist. *)
val get_decl_by_id : env -> decl_id -> decl option

(** Retrieve a declaration by its sourced name, returning {!None} if it doesn't exist.

    If a module with the given name doesn't exist, raises an error. *)
val get_decl_by_name : env -> sident -> decl option
