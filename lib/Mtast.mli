(** The monomorphic AST. *)
open Identifier
open Common
open Escape
open Span
open Semantic
open MonoType

(** Represents a module where all types and functions have been monomorphized. *)
type mono_module = MonoModule of module_name * mtyped_decl list

(** Declarations in a monomorphic module. *)
and mtyped_decl =
  | MConstant of identifier * mono_ty * mexpr
  | MRecord of identifier * mtyped_slot list
  | MUnion of identifier * mtyped_case list
  | MFunction of identifier * mvalue_parameter list * mono_ty * mstmt
  | MForeignFunction of identifier * mvalue_parameter list * mono_ty * string

(** Monomorphic statements. *)
and mstmt =
  | MSkip of span
  | MLet of span * identifier * mono_ty * mexpr * mstmt
  | MDestructure of span * (identifier * mono_ty) list * mexpr * mstmt
  | MAssign of span * mtyped_lvalue * mexpr
  | MIf of span * mexpr * mstmt * mstmt
  | MCase of span * mexpr * mtyped_when list
  | MWhile of span * mexpr * mstmt
  | MFor of span * identifier * mexpr * mexpr * mstmt
  | MBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: mono_ty;
      ref_type: mono_ty;
      body: mstmt;
      mode: borrowing_mode
    }
  | MBlock of span * mstmt * mstmt
  | MDiscarding of span * mexpr
  | MReturn of span * mexpr

(** Monomorphic expressions. *)
and mexpr =
  | MNilConstant
  | MBoolConstant of bool
  | MIntConstant of string
  | MFloatConstant of string
  | MStringConstant of escaped_string
  | MVariable of qident * mono_ty
  | MArithmetic of arithmetic_operator * mexpr * mexpr
  | MFuncall of qident * mexpr list * mono_ty * (identifier * mono_ty) list
  | MMethodCall of qident * semantic_instance * mexpr list * mono_ty
  | MCast of mexpr * mono_ty
  | MComparison of comparison_operator * mexpr * mexpr
  | MConjunction of mexpr * mexpr
  | MDisjunction of mexpr * mexpr
  | MNegation of mexpr
  | MIfExpression of mexpr * mexpr * mexpr
  | MRecordConstructor of mono_ty * (identifier * mexpr) list
  | MUnionConstructor of mono_ty * identifier * (identifier * mexpr) list
  | MPath of {
      head: mexpr;
      elems: mtyped_path_elem list;
      ty: mono_ty
    }
  | MEmbed of mono_ty * string * mexpr list
  | MDeref of mexpr
  | MTypecast of mexpr * mono_ty
  | MSizeOf of mono_ty

and mtyped_when =
  MTypedWhen of identifier * mvalue_parameter list * mstmt

and mtyped_path_elem =
  | MSlotAccessor of identifier * mono_ty
  | MPointerSlotAccessor of identifier * mono_ty
  | MArrayIndex of mexpr * mono_ty

and mtyped_lvalue =
  MTypedLValue of identifier * mtyped_path_elem list

and mvalue_parameter =
  MValueParameter of identifier * mono_ty

and mtyped_slot =
  MTypedSlot of identifier * mono_ty

and mtyped_case =
  MTypedCase of identifier * mtyped_slot list
