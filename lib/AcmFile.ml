(** Types for representing Austral Compiled Module (.acm) files. *)
open Identifier
open Id
open ModIdSet
open Type
open TypeParameters
open Sexplib
open Std
open Span
open Common
open Escape
open Region

(** Types in ACM files are always either public or opaque. *)
type acm_type_vis =
  | AcmTypeVisPublic
  | AcmTypeVisOpaque
[@@deriving sexp]

(** The serialized form of a statement. *)
type ser_stmt =
  | TSkip of span
  | TLet of span * identifier * ty * ser_expr * ser_stmt
  | TDestructure of span * (identifier * ty) list * ser_expr * ser_stmt
  | TAssign of span * ser_lvalue * ser_expr
  | TIf of span * ser_expr * ser_stmt * ser_stmt
  | TCase of span * ser_expr * ser_when list
  | TWhile of span * ser_expr * ser_stmt
  | TFor of span * identifier * ser_expr * ser_expr * ser_stmt
  | TBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: ser_stmt;
      mode: borrowing_mode
    }
  | TBlock of span * ser_stmt * ser_stmt
  | TDiscarding of span * ser_expr
  | TReturn of span * ser_expr
[@@deriving sexp]

(** A reference to an instance. *)
and ins_ref =
  InsRef of {
      module_name: module_name;
      typeclass_name: identifier;
      argument: ty;
    }
[@@deriving sexp]

(** A reference to an instance method. *)
and ins_meth_ref =
  InsMethRef of ins_ref * identifier
[@@deriving sexp]

(** The serialized form of an expression. *)
and ser_expr =
  | TNilConstant
  | TBoolConstant of bool
  | TIntConstant of string
  | TFloatConstant of string
  | TStringConstant of escaped_string
  | TConstVar of qident * ty
  | TParamVar of identifier * ty
  | TLocalVar of identifier * ty
  | TArithmetic of arithmetic_operator * ser_expr * ser_expr
  | TFuncall of qident * ser_expr list * ty * (identifier * ty) list
  | TMethodCall of ins_meth_ref * qident * typarams * ser_expr list * ty * (identifier * ty) list
  | TCast of ser_expr * ty
  | TComparison of comparison_operator * ser_expr * ser_expr
  | TConjunction of ser_expr * ser_expr
  | TDisjunction of ser_expr * ser_expr
  | TNegation of ser_expr
  | TIfExpression of ser_expr * ser_expr * ser_expr
  | TRecordConstructor of ty * (identifier * ser_expr) list
  | TUnionConstructor of ty * identifier * (identifier * ser_expr) list
  | TTypeAliasConstructor of ty * ser_expr
  | TPath of {
      head: ser_expr;
      elems: ser_path_elem list;
      ty: ty
    }
  | TEmbed of ty * string * ser_expr list
  | TDeref of ser_expr
  | TSizeOf of ty
  | TBorrowExpr of borrowing_mode * identifier * region * ty
[@@deriving sexp]

and ser_when =
  TypedWhen of identifier * value_parameter list * ser_stmt
[@@deriving sexp]

and ser_path_elem =
  | TSlotAccessor of identifier * ty
  | TPointerSlotAccessor of identifier * ty
  | TArrayIndex of ser_expr * ty
[@@deriving sexp]

and ser_lvalue =
  TypedLValue of identifier * ser_path_elem list
[@@deriving sexp]

and ser_arglist =
  | TPositionalArglist of ser_expr list
  | TNamedArglist of (identifier * ser_expr) list
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
      body: ser_stmt option;
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
      body: ser_stmt;
    }
[@@deriving sexp]

(** Represents the contents of an ACM file. *)
type compiled_module =
  CompiledModule of {
      name: module_name;
      imports_from: ModIdSet.t;
      decls: compiled_decl list;
    }
