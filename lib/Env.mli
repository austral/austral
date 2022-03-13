open Identifier
open Common
open Type
open Tast
open Id
open LexEnv

(** {1 Types} *)

(** A set of type parameters. *)
type typarams = type_parameter list

(** The global compiler environment. *)
type env

(** A file record contains a file's path and contents. *)
type file_rec = FileRec of { id: file_id; path: string; contents: string }

(** Type of the input to the {!add_file} function. *)
type file_input = { path: string; contents: string }

(** Type of the input to the {!add_module} function. *)
type mod_input = {
    name: module_name;
    interface_file: file_id;
    interface_docstring: docstring;
    body_file: file_id;
    body_docstring: docstring;
    kind: module_kind
  }

(** A module record contains a module's name, docstring, and pointers to the
   interface and body files. *)
type mod_rec = ModRec of {
      id: mod_id;
      name: module_name;
      interface_file: file_id;
      interface_docstring: docstring;
      body_file: file_id;
      body_docstring: docstring;
      kind: module_kind
    }

(** Input to the {!add_const} function. *)
type const_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    ty: ty;
    docstring: docstring;
  }

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

(** Input to the {!add_union} function. *)
type union_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

(** Input to the {!add_union_case} function. *)
type union_case_input = {
    mod_id: mod_id;
    vis: vis;
    union_id: decl_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

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

(** Input to the {!add_type_class} function. *)
type type_class_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

(** Input to the {!add_type_class_method} function. *)
type type_class_method_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    name: identifier;
    docstring: docstring;
    value_params: value_parameter list;
    rt: ty;
  }

(** Input to the {!add_instance} function. *)
type instance_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

(** Input to the {!add_instance_method} function. *)
type instance_method_input = {
    instance_id: decl_id;
    method_id: decl_id;
    docstring: docstring;
    name: identifier;
    value_params: value_parameter list;
    rt: ty;
    body: tstmt option;
  }

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
      (** This is {!VisPublic} if the parent union is public, {!VisPrivate} otherwise. *)
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
      (** This is the visibility of the parent typeclass. *)
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

(** Callable things. *)
type callable =
  | FunctionCallable of type_parameter list * value_parameter list * ty
  | TypeAliasCallable of type_parameter list * universe * ty
  | RecordConstructor of type_parameter list * universe * typed_slot list
  | UnionConstructor of {
      union_id: decl_id;
      type_params: type_parameter list;
      universe: universe;
      case: typed_case;
    }
  | MethodCallable of {
      typeclass_id: decl_id;
      value_parameters: value_parameter list;
      return_type: ty
    }

(** {1 Constants} *)

(** The empty compiler. *)
val empty_env : env

(** {1 Functions} *)

(** {2 Insertion Functions} *)

(** Add a file's path and contents to the environment. *)
val add_file : env -> file_input -> (env * file_id)

(** Add a module's information to the environment. *)
val add_module : env -> mod_input -> (env * mod_id)

val add_constant : env -> const_input -> (env * decl_id)

val add_type_alias : env -> type_alias_input -> (env * decl_id)

val add_record : env -> record_input -> (env * decl_id)

val add_union : env -> union_input -> (env * decl_id)

val add_union_case : env -> union_case_input -> (env * decl_id)

val add_function : env -> function_input -> (env * decl_id)

val add_type_class : env -> type_class_input -> (env * decl_id)

val add_type_class_method : env -> type_class_method_input -> (env * decl_id)

val add_instance : env -> instance_input -> (env * decl_id)

val add_instance_method : env -> instance_method_input -> (env * ins_meth_id)

(** Store the given function body in the function with the given ID, returning
    the new environment. *)
val store_function_body : env -> decl_id -> tstmt -> env

(** Store the given instance method body in the instance method with the given
    ID, returning the new environment. *)
val store_method_body : env -> ins_meth_id -> tstmt -> env

(** {2 Retrieval Functions} *)

(** Retrieve a module by its module ID. *)
val get_module_by_id : env -> mod_id -> mod_rec option

(** Retrieve a module by its name. *)
val get_module_by_name : env -> module_name -> mod_rec option

(** Retrieve a declaration by ID, returning {!None} if it doesn't exist. *)
val get_decl_by_id : env -> decl_id -> decl option

(** Retrieve a declaration by its sourced name, returning {!None} if it doesn't exist.

    If a module with the given name doesn't exist, raises an error. *)
val get_decl_by_name : env -> sident -> decl option

(** Find a typeclass method from its typeclass ID and name. *)
val get_method_from_typeclass_id_and_name : env -> decl_id -> identifier -> decl option

(** Return the typeclass instances defined in a module. *)
val module_instances: env -> mod_id -> decl list

(** Return a module's name from its ID. *)
val module_name_from_id : env -> mod_id -> module_name

(** Return the list of cases of a union. *)
val get_union_cases : env -> decl_id -> decl list

(** Get a callable given its name and the name of the importing module. *)
val get_callable : env -> module_name -> sident -> callable option

(** Get the type of a variable, trying first the lexenv and then the env for
    constants. *)
val get_variable : env -> lexenv -> qident -> ty option

(** Return all typeclass instances visible from a module. These are not only the
    instances that are defined in the module itself, but the instances imported
    by the module. *)
val visible_instances : env -> decl list

(** {2 Other Functions} *)

(** Return the ID of a declaration. *)
val decl_id : decl -> decl_id

(** Return whether a declaration is importable by a foreign module. *)
val is_importable: decl -> bool

val union_case_to_typed_case : decl -> typed_case

(** Get method from the instance ID and name. *)
val get_instance_method_from_instance_id_and_method_name : env -> decl_id -> identifier -> ins_meth_rec option
