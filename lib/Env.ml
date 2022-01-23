open Identifier
open Common
open Type
open Tast

(** A set of type parameters. *)
type typarams = type_parameter list

(** The type of file IDs. *)
type file_id = FileId of int

(** The type of module IDs. *)
type mod_id = ModId of int

(** The type of declaration IDs. *)
type decl_id = DeclId of int

(** The type of instance method IDs. *)
type ins_meth_id = InsMethId of int

(** A file record contains a file's path and contents. *)
type file_rec = FileRec of { id: file_id; path: string; contents: string }

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

(** A declaration record represents a declaration.

   The definition of declaration here is slightly different than that of the
   spec: we include union cases and typeclass methods as "declarations". The
   reason being that we want union cases and typeclass methods to share a
   namespace with real declarations, and this is an easy way to make that
   happen.

*)
type decl_rec =
  | Constant of {
      id: decl_id;
      mod_id: mod_id;
      name: identifier;
      docstring: docstring;
    }
  | TypeAlias of {
      id: decl_id;
      mod_id: mod_id;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      universe: universe;
      def: ty;
    }
  | Record of {
      id: decl_id;
      mod_id: mod_id;
      name: identifier;
      docstring: docstring;
      typarams: typarams;
      universe: universe;
      slots: typed_slot list;
    }
  | Union of {
      id: decl_id;
      mod_id: mod_id;
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

(** Represents an instance method. *)
type ins_meth_rec = InsMethRec of {
      id: ins_meth_id;
      instance_id: decl_id;
      docstring: docstring;
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
type decl_env = decl_rec list

(** The env type implements the global compiler environment. *)
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

(** An empty environment. *)
let empty_env: env =
  Env { files = []; mods = []; methods = []; decls = [] }

(** Add a file to an environment. *)
let add_file (env: env) (path: string) (contents: string): (env * file_id) =
  let (Env { files; mods; methods; decls }) = env in
  let id = fresh_file_id () in
  let file = FileRec { id = id; path = path; contents = contents } in
  let env = Env { files = file :: files; mods = mods; methods = methods; decls = decls } in
  (env, id)
