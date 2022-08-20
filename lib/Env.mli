open Identifier
open Type
open MonoType
open MonoTypeBindings
open Tast
open Mtast
open Id
open LexEnv
open EnvTypes

(** {1 Types} *)

(** The global environment type. *)
type env

(** {1 Constants} *)

(** The empty compiler. *)
val empty_env : env

(** {1 File Functions} *)

(** Add a file's path and contents to the environment. *)
val add_file : env -> file_input -> (env * file_id)

(** {1 Module Functions} *)

(** Add a module's information to the environment. *)
val add_module : env -> mod_input -> (env * mod_id)

(** Retrieve a module by its module ID. *)
val get_module_by_id : env -> mod_id -> mod_rec option

(** Retrieve a module by its name. *)
val get_module_by_name : env -> module_name -> mod_rec option

(** Return a module's name from its ID. *)
val module_name_from_id : env -> mod_id -> module_name

(** {1 Declaration Functions} *)

(** {2 Create} *)

val add_constant : env -> const_input -> (env * decl_id)

val add_record : env -> record_input -> (env * decl_id)

val add_union : env -> union_input -> (env * decl_id)

val add_union_case : env -> union_case_input -> (env * decl_id)

val add_function : env -> function_input -> (env * decl_id)

val add_type_class : env -> type_class_input -> (env * decl_id)

val add_type_class_method : env -> type_class_method_input -> (env * decl_id)

val add_instance : env -> instance_input -> (env * decl_id)

val add_instance_method : env -> instance_method_input -> (env * ins_meth_id)

(** {2 Retrieve} *)

(** Retrieve a declaration by ID, returning {!None} if it doesn't exist. *)
val get_decl_by_id : env -> decl_id -> decl option

(** Retrieve a declaration by its sourced name, returning {!None} if it doesn't exist.

    If a module with the given name doesn't exist, raises an error. *)
val get_decl_by_name : env -> sident -> decl option

(** Find a typeclass method from its typeclass ID and name. *)
val get_method_from_typeclass_id_and_name : env -> decl_id -> identifier -> decl option

(** Return all typeclass instances defined in a module. *)
val module_instances: env -> mod_id -> decl list

(** Return the public typeclass instances defined in a module. *)
val module_public_instances: env -> mod_id -> decl list

(** Return the list of cases of a union. *)
val get_union_cases : env -> decl_id -> decl list

(** Get method from the instance ID and name. *)
val get_instance_method_from_instance_id_and_method_name : env -> decl_id -> identifier -> ins_meth_rec option

(** Get an instance method by ID. *)
val get_instance_method : env -> ins_meth_id -> ins_meth_rec option

(** {2 Update} *)

(** Store the given function body in the function with the given ID, returning
    the new environment. *)
val store_function_body : env -> decl_id -> tstmt -> env

(** Store the given instance method body in the instance method with the given
    ID, returning the new environment. *)
val store_method_body : env -> ins_meth_id -> tstmt -> env

(** {1 Callable Functions} *)

(** Get a callable given its name and the name of the importing module. *)
val get_callable : env -> module_name -> sident -> callable option

(** {1 Monomorph Functions} *)

val add_record_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

val add_union_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

val add_function_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

val add_instance_method_monomorph : env -> ins_meth_id -> mono_type_bindings -> (env * mono_id)

(** Given a record's ID and a list of arguments, register a monomorph and return
   its ID, or return the ID if it exists. *)
val add_or_get_record_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

(** Given a union's ID and a list of arguments, register a monomorph and return
   its ID, or return the ID if it exists. *)
val add_or_get_union_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

(** Given a function's ID and a list of type arguments, register a monomorph and
   return its ID, or return the ID if it exists. *)
val add_or_get_function_monomorph : env -> decl_id -> mono_type_bindings -> (env * mono_id)

(** Given an instance method's ID and a list of type arguments, register a
   monomorph and return its ID, or return the ID if it exists. *)
val add_or_get_instance_method_monomorph : env -> ins_meth_id -> mono_type_bindings -> (env * mono_id)

val store_record_monomorph_definition : env -> mono_id -> mono_slot list -> env

val store_union_monomorph_definition : env -> mono_id -> mono_case list -> env

val store_function_monomorph_definition : env -> mono_id -> mstmt -> env

val store_instance_method_monomorph_definition : env -> mono_id -> mstmt -> env

(** Given the ID of a type declaration and a set of monomorphic type arguments,
   return the ID of the corresponding monomorph if it exists. *)
val get_type_monomorph : env -> decl_id -> mono_type_bindings -> mono_id option

(** Given the ID of a function and a set of monomorphic type arguments, return
   the ID of the corresponding monomorph if it exists. *)
val get_function_monomorph : env -> decl_id -> mono_type_bindings -> mono_id option

(** Given the ID of an instance method and a set of monomorphic type arguments,
   return the ID of the corresponding monomorph if it exists. *)
val get_instance_method_monomorph : env -> ins_meth_id -> mono_type_bindings -> mono_id option

(** Get all uninstantiated monomorphs. *)
val get_uninstantiated_monomorphs : env -> monomorph list

(** Get a monomorph by ID. *)
val get_monomorph : env -> mono_id -> monomorph option

(** Get the type of a variable, trying first the lexenv and then the env for
    constants. *)
val get_variable : env -> lexenv -> qident -> (ty * var_source) option

(** Return all typeclass instances visible from a module. These are not only the
    instances that are defined in the module itself, but the instances imported
    by the module. *)
val visible_instances : env -> mod_id -> decl list
