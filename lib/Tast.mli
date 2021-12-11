open Identifier
open Common
open Escape
open Type
open Span

type typed_module = TypedModule of module_name * typed_decl list

and typed_decl =
  | TConstant of vis * identifier * ty * texpr * docstring
  | TTypeAlias of type_vis * identifier * type_parameter list * universe * ty * docstring
  | TRecord of type_vis * identifier * type_parameter list * universe * typed_slot list * docstring
  | TUnion of type_vis * identifier * type_parameter list * universe * typed_case list * docstring
  | TFunction of vis * identifier * type_parameter list * value_parameter list * ty * tstmt * docstring
  | TForeignFunction of vis * identifier * value_parameter list * ty * string * docstring
  | TTypeClass of vis * identifier * type_parameter * typed_method_decl list * docstring
  | TInstance of vis * identifier * type_parameter list * ty * typed_method_def list * docstring

and typed_method_decl =
  TypedMethodDecl of identifier * value_parameter list * ty

and typed_method_def =
  TypedMethodDef of identifier * value_parameter list * ty * tstmt

and tstmt =
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

and texpr =
  | TNilConstant
  | TBoolConstant of bool
  | TIntConstant of string
  | TFloatConstant of string
  | TStringConstant of escaped_string
  | TVariable of qident * ty
  | TArithmetic of arithmetic_operator * texpr * texpr
  | TFuncall of qident * texpr list * ty * (identifier * ty) list
  | TMethodCall of qident * type_parameter list * texpr list * ty * (identifier * ty) list
  | TCast of texpr * ty
  | TComparison of comparison_operator * texpr * texpr
  | TConjunction of texpr * texpr
  | TDisjunction of texpr * texpr
  | TNegation of texpr
  | TIfExpression of texpr * texpr * texpr
  | TRecordConstructor of ty * (identifier * texpr) list
  | TUnionConstructor of ty * identifier * (identifier * texpr) list
  | TPath of {
      head: texpr;
      elems: typed_path_elem list;
      ty: ty
    }
  | TEmbed of ty * string * texpr list
  | TDeref of texpr
  | TSizeOf of ty

and typed_when =
  TypedWhen of identifier * value_parameter list * tstmt

and typed_path_elem =
  | TSlotAccessor of identifier * ty
  | TPointerSlotAccessor of identifier * ty
  | TArrayIndex of texpr * ty

and typed_lvalue =
  TypedLValue of identifier * typed_path_elem list

type typed_arglist =
  | TPositionalArglist of texpr list
  | TNamedArglist of (identifier * texpr) list

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
