open StringSet
open Identifier
open Common
open Escape
open Type
open Span
open Error

type typed_module = TypedModule of module_name * typed_decl list

and typed_decl =
  | TConstant of vis * identifier * ty * texpr * docstring
  | TTypeAlias of type_vis * identifier * type_parameter list * universe * ty * docstring
  | TRecord of type_vis * identifier * type_parameter list * universe * typed_slot list * docstring
  | TUnion of type_vis * identifier * type_parameter list * universe * typed_case list * docstring
  | TFunction of vis * identifier * type_parameter list * value_parameter list * ty * tstmt * docstring
  | TForeignFunction of vis * identifier * value_parameter list * ty * string * docstring
  | TTypeClass of vis * identifier * type_parameter * typed_method_decl list * docstring
  | TInstance of vis * qident * type_parameter list * ty * typed_method_def list * docstring

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
  | TTypeAliasConstructor of ty * texpr
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

let arglist_size = function
  | (TPositionalArglist l) -> List.length l
  | (TNamedArglist l) -> List.length l

let rec arglist_to_positional (args, names) =
  assert ((arglist_size args) = (List.length names));
  assert (names_match args names);
  match args with
  | (TPositionalArglist l) -> l
  | (TNamedArglist l) -> reorder_arglist l names

and names_match arglist names =
  match arglist with
  | (TPositionalArglist _) -> true
  | (TNamedArglist l) -> names_match_inner (List.map (fun (n, _) -> ident_string n) l) (List.map ident_string names)

and names_match_inner a b =
  StringSet.equal (StringSet.of_list a) (StringSet.of_list b)

and reorder_arglist l names =
  List.map (fun n -> find_arg_with_name l n) names

and find_arg_with_name l name =
  let (_, value) = List.find (fun (n, _) -> name = n) l in
  value

let rec get_type = function
  | TNilConstant ->
     Unit
  | TBoolConstant _ ->
     Boolean
  | TIntConstant _ ->
     Integer (Signed, Width32)
  | TFloatConstant _ ->
     DoubleFloat
  | TStringConstant _ ->
     string_type
  | TVariable (_, ty) ->
     ty
  | TArithmetic (_, lhs, _) ->
     get_type lhs
  | TFuncall (_, _, ty, _) ->
     ty
  | TMethodCall (_, _, _, ty, _) ->
     ty
  | TCast (_, ty) ->
     ty
  | TComparison _ ->
     Boolean
  | TConjunction _ ->
     Boolean
  | TDisjunction _ ->
     Boolean
  | TNegation _ ->
     Boolean
  | TIfExpression (_, t, _) ->
     get_type t
  | TRecordConstructor (ty, _) ->
     ty
  | TUnionConstructor (ty, _, _) ->
     ty
  | TTypeAliasConstructor (ty, _) ->
     ty
  | TPath { ty; _ } ->
     ty
  | TEmbed (ty, _, _) ->
     ty
  | TDeref e ->
     (match get_type e with
      | ReadRef (t, _) ->
         t
      | WriteRef (t, _) ->
         t
      | _ ->
         err ("Internal error: a dereference expression was constructed whose argument is not a reference type."))
  | TSizeOf _ ->
     Integer (Unsigned, Width64)

and path_elem_type = function
  | TSlotAccessor (_, t) ->
     t
  | TPointerSlotAccessor (_, t) ->
     t
  | TArrayIndex (_, t) ->
     t
