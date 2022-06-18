(** Types used in the various environments. *)
open Id
open DeclIdSet
open ModIdSet
open Identifier
open Common
open Type
open TypeParameters
open Tast
open MonoType
open Mtast

(** A file record contains a file's path and contents. *)
type file_rec = FileRec of { id: file_id; path: string; contents: string }

type file_input = { path: string; contents: string }

(** A module record contains a module's name, docstring, and pointers to the
   interface and body files. *)
type mod_rec = ModRec of {
      id: mod_id;
      name: module_name;
      interface_file: file_id;
      interface_docstring: docstring;
      body_file: file_id;
      body_docstring: docstring;
      kind: module_kind;
      imported_instances: DeclIdSet.t;
      imports_from: ModIdSet.t;
    }

type mod_input = {
    name: module_name;
    interface_file: file_id;
    interface_docstring: docstring;
    body_file: file_id;
    body_docstring: docstring;
    kind: module_kind;
    imported_instances: DeclIdSet.t;
    imports_from: ModIdSet.t;
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

(** Represents a monomorph. *)
type monomorph =
  | MonoTypeAliasDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      def: mono_ty option;
      (** The definition of the type alias, present once it's instantiated. *)
    }
  | MonoRecordDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      slots: (mono_slot list) option;
      (** The list of slots, if instantiated. *)
    }
  | MonoUnionDefinition of {
      id: mono_id;
      type_id: decl_id;
      tyargs: mono_ty list;
      cases: (mono_case list) option;
      (** The list of cases, if instantiated. *)
    }
  | MonoFunction of {
      id: mono_id;
      function_id: decl_id;
      tyargs: mono_ty list;
      body: mstmt option;
      (** The function body, if instantiated. *)
    }
  | MonoInstanceMethod of {
      id: mono_id;
      method_id: ins_meth_id;
      tyargs: mono_ty list;
      body: mstmt option;
    }

type const_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    ty: ty;
    docstring: docstring;
  }

type type_alias_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    def: ty;
  }

type record_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
    slots: typed_slot list;
  }

type union_input = {
    mod_id: mod_id;
    vis: type_vis;
    name: identifier;
    docstring: docstring;
    typarams: typarams;
    universe: universe;
  }

type union_case_input = {
    mod_id: mod_id;
    vis: vis;
    union_id: decl_id;
    name: identifier;
    docstring: docstring;
    slots: typed_slot list;
  }

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

type type_class_input = {
    mod_id: mod_id;
    vis: vis;
    name: identifier;
    docstring: docstring;
    param: type_parameter;
  }

type type_class_method_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    name: identifier;
    docstring: docstring;
    value_params: value_parameter list;
    rt: ty;
  }

type instance_input = {
    mod_id: mod_id;
    vis: vis;
    typeclass_id: decl_id;
    docstring: docstring;
    typarams: typarams;
    argument: ty;
  }

type instance_method_input = {
    instance_id: decl_id;
    method_id: decl_id;
    docstring: docstring;
    name: identifier;
    value_params: value_parameter list;
    rt: ty;
    body: tstmt option;
  }
