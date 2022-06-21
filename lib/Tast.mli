open Identifier
open Common
open Escape
open Type
open Region
open TypeParameters
open TypeBindings
open Linked
open Id
open Span

type tstmt =
  | TSkip of span
  | TLet of span * identifier * ty * texpr * tstmt
  | TDestructure of span * (identifier * ty) list * texpr * tstmt
  | TAssign of span * typed_lvalue * texpr
  | TIf of span * texpr * tstmt * tstmt
  | TCase of span * texpr * typed_when list
  | TWhile of span * texpr * tstmt
  | TFor of span * identifier * texpr * texpr * tstmt
  | TBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: tstmt;
      mode: borrowing_mode
    }
  | TBlock of span * tstmt * tstmt
  | TDiscarding of span * texpr
  | TReturn of span * texpr
[@@deriving show]

and texpr =
  | TNilConstant
  | TBoolConstant of bool
  | TIntConstant of string
  | TFloatConstant of string
  | TStringConstant of escaped_string
  | TConstVar of qident * ty
  | TParamVar of identifier * ty
  | TLocalVar of identifier * ty
  | TArithmetic of arithmetic_operator * texpr * texpr
  | TFuncall of decl_id * qident * texpr list * ty * (identifier * ty) list
  | TMethodCall of ins_meth_id * qident * typarams * texpr list * ty * (identifier * ty) list
  | TVarMethodCall of {
      source_module_name: module_name;
      typeclass_id: decl_id;
      params: value_parameter list;
      method_name: qident;
      args: texpr list;
      dispatch_ty: ty;
      rt: ty;
      bindings: type_bindings;
    }
  | TCast of texpr * ty
  | TComparison of comparison_operator * texpr * texpr
  | TConjunction of texpr * texpr
  | TDisjunction of texpr * texpr
  | TNegation of texpr
  | TIfExpression of texpr * texpr * texpr
  | TRecordConstructor of ty * (identifier * texpr) list
  | TUnionConstructor of ty * identifier * (identifier * texpr) list
  | TTypeAliasConstructor of ty * texpr
  | TPath of {
      head: texpr;
      elems: typed_path_elem list;
      ty: ty
    }
  | TEmbed of ty * string * texpr list
  | TDeref of texpr
  | TSizeOf of ty
  | TBorrowExpr of borrowing_mode * identifier * region * ty
[@@deriving show]

and typed_when =
  TypedWhen of identifier * value_parameter list * tstmt
[@@deriving show]

and typed_path_elem =
  | TSlotAccessor of identifier * ty
  | TPointerSlotAccessor of identifier * ty
  | TArrayIndex of texpr * ty
[@@deriving show]

and typed_lvalue =
  TypedLValue of identifier * typed_path_elem list
[@@deriving show]

and typed_arglist =
  | TPositionalArglist of texpr list
  | TNamedArglist of (identifier * texpr) list
[@@deriving show]

type typed_method_decl =
  TypedMethodDecl of decl_id * identifier * value_parameter list * ty

type typed_method_def =
  TypedMethodDef of ins_meth_id * identifier * value_parameter list * ty * tstmt

type typed_decl =
  | TConstant of decl_id * vis * identifier * ty * texpr * docstring
  | TTypeAlias of decl_id * type_vis * identifier * typarams * universe * ty * docstring
  | TRecord of decl_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
  | TUnion of decl_id * type_vis * identifier * typarams * universe * linked_case list * docstring
  | TFunction of decl_id * vis * identifier * typarams * value_parameter list * ty * tstmt * docstring
  | TForeignFunction of decl_id * vis * identifier * value_parameter list * ty * string * docstring
  | TTypeClass of decl_id * vis * identifier * type_parameter * typed_method_decl list * docstring
  | TInstance of decl_id * vis * qident * typarams * ty * typed_method_def list * docstring

type typed_module = TypedModule of module_name * typed_decl list

val arglist_size : typed_arglist -> int

(* This function converts an argument list to a list of typed expressions.

   The second argument is the list of parameter names of the function being
   called.

   If the argument list is a positional argument list, we return list is
   returned unchanged.

   If this is a named argument list, we convert it to a positional list using
   the given list of parameter names.

   For example, if the given list of parameter names is ["a", "b", "c"], and the
   list is a named list with a map like {"b" => 3, "a" => 3.14, "c" =>
   "hello!"}, this returns the list [3.14, 3, "hello!"].

   Checks:

       1. The list of parameter names has the same size as the argument
          list.
       2. If the argument list is a named argument list, then the set of keys
          is exactly the same as the set of given parameter names.
*)
val arglist_to_positional : typed_arglist * identifier list -> texpr list

val get_type : texpr -> ty

val path_elem_type : typed_path_elem -> ty
