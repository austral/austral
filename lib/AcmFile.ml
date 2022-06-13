(** Types for representing Austral Compiled Module (.acm) files. *)
open Identifier
open Id
open ModIdSet
open Type
open TypeParameters
open Tast

(** Types in ACM files are always either public or opaque. *)
type acm_type_vis =
  | AcmTypeVisPublic
  | AcmTypeVisOpaque
[@@deriving sexp]

type compiled_decl =
  | CompiledConstant of {
      name: identifier;
      ty: ty;
    }
  (** Constants in ACM files are always public. *)
  | CompiledTypeAlias of {
      vis: acm_type_vis;
      name: identifier;
      typarams: typarams;
      universe: universe;
      def: ty;
    }
  | CompiledRecord of {
      vis: acm_type_vis;
      name: identifier;
      typarams: typarams;
      universe: universe;
      slots: typed_slot list;
    }
  | CompiledUnion of {
      vis: acm_type_vis;
      name: identifier;
      typarams: typarams;
      universe: universe;
      cases: typed_case list;
    }
  | CompiledFunction of {
      name: identifier;
      typarams: typarams;
      value_params: value_parameter list;
      rt: ty;
      external_name: string option;
      (** If this function is foreign, this is the name of the underlying function
          that will be called. *)
      body: tstmt option;
      (** If the function is generic, it should have a body. *)
    }
  (** Functions in ACM files are always public. *)
  | CompiledTypeClass of {
      mod_id: mod_id;
      name: identifier;
      param: type_parameter;
      methods: compiled_method_decl list;
    }
  (** Type classes in ACM files are always public. *)
  | CompiledInstance of {
      typeclass_name: sident;
      typarams: typarams;
      argument: ty;
      methods: compiled_method_def list;
    }
  (** Instances in ACM files are always public. *)

and compiled_method_decl =
  CompiledMethodDecl of {
      name: identifier;
      value_params: value_parameter list;
      rt: ty;
    }

and compiled_method_def =
  CompiledMethodDef of {
      name: identifier;
      value_params: value_parameter list;
      rt: ty;
      body: tstmt;
    }
[@@deriving sexp]

(** Represents the contents of an ACM file. *)
type compiled_module =
  CompiledModule of {
      name: module_name;
      imports_from: ModIdSet.t;
      decls: compiled_decl list;
    }
