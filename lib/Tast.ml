open StringSet
open Identifier
open Common
open Escape
open Type
open TypeParameters
open Span
open Linked
open Id
open Region
open Error

type tstmt =
  | TSkip of span
  | TLet of span * identifier * ty * texpr * tstmt
  | TLetBorrow of {
      span: span;
      name: identifier;
      ty: ty;
      region_name: identifier;
      region: region;
      var_name: identifier;
      mode: borrowing_mode;
      body: tstmt;
    }
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
  | TConstVar (_, ty) ->
     ty
  | TParamVar (_, ty) ->
     ty
  | TLocalVar (_, ty) ->
     ty
  | TArithmetic (_, lhs, _) ->
     get_type lhs
  | TFuncall (_, _, _, ty, _) ->
     ty
  | TMethodCall (_, _, _, _, ty, _) ->
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
     Integer (Unsigned, WidthIndex)
  | TBorrowExpr (mode, _, region, ty) ->
     (match mode with
      | ReadBorrow ->
         ReadRef (ty, RegionTy region)
      | WriteBorrow ->
         WriteRef (ty, RegionTy region))

and path_elem_type = function
  | TSlotAccessor (_, t) ->
     t
  | TPointerSlotAccessor (_, t) ->
     t
  | TArrayIndex (_, t) ->
     t
