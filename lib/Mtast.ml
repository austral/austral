open Id
open Identifier
open Common
open Escape
open MonoType

type mono_module = MonoModule of module_name * mdecl list

and mdecl =
  | MConstant of decl_id * identifier * mono_ty * mexpr
  | MTypeAlias of decl_id * identifier * mono_ty
  | MRecord of decl_id * identifier * mono_slot list
  | MUnion of decl_id * identifier * mono_case list
  | MFunction of decl_id * identifier * mvalue_parameter list * mono_ty * mstmt
  | MForeignFunction of decl_id * identifier * mvalue_parameter list * mono_ty * string
  | MConcreteInstance of decl_id * qident * mono_ty  * concrete_method list

and concrete_method =
  MConcreteMethod of ins_meth_id * identifier * mvalue_parameter list * mono_ty * mstmt

and mstmt =
  | MSkip
  | MLet of identifier * mono_ty * mexpr * mstmt
  | MDestructure of (identifier * mono_ty) list * mexpr * mstmt
  | MAssign of mtyped_lvalue * mexpr
  | MIf of mexpr * mstmt * mstmt
  | MCase of mexpr * mtyped_when list
  | MWhile of mexpr * mstmt
  | MFor of identifier * mexpr * mexpr * mstmt
  | MBorrow of {
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: mono_ty;
      ref_type: mono_ty;
      body: mstmt;
      mode: borrowing_mode
    }
  | MBlock of mstmt * mstmt
  | MDiscarding of mexpr
  | MReturn of mexpr

and mexpr =
  | MNilConstant
  | MBoolConstant of bool
  | MIntConstant of string
  | MFloatConstant of string
  | MStringConstant of escaped_string
  | MVariable of qident * mono_ty
  | MArithmetic of arithmetic_operator * mexpr * mexpr
  | MConcreteFuncall of decl_id * qident * mexpr list * mono_ty
  (** Represents a call to a concrete function. *)
  | MGenericFuncall of mono_id * mexpr list * mono_ty
  (** Represents a call to a generic function. *)
  | MConcreteMethodCall of ins_meth_id * qident * mexpr list * mono_ty
  (** Represents a call to an instance method of a concrete instance. *)
  | MGenericMethodCall of ins_meth_id * mono_id * mexpr list * mono_ty
  (** Represents a call to an instance method of a generic instance. *)
  | MCast of mexpr * mono_ty
  | MComparison of comparison_operator * mexpr * mexpr
  | MConjunction of mexpr * mexpr
  | MDisjunction of mexpr * mexpr
  | MNegation of mexpr
  | MIfExpression of mexpr * mexpr * mexpr
  | MRecordConstructor of mono_ty * (identifier * mexpr) list
  | MUnionConstructor of mono_ty * identifier * (identifier * mexpr) list
  | MTypeAliasConstructor of mono_ty * mexpr
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

type mono_method =
  MonoMethod of {
      method_id: ins_meth_id;
      params: mvalue_parameter list;
      rt: mono_ty;
      body: mstmt;
    }
