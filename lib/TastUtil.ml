open Type
open Tast
open Identifier
open StringSet
open Error

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
  | TFunVar (_, ty, _) ->
     ty
  | TArithmetic (_, lhs, _) ->
     get_type lhs
  | TFuncall (_, _, _, ty, _) ->
     ty
  | TMethodCall (_, _, _, _, ty, _) ->
     ty
  | TVarMethodCall { rt; _ } ->
     rt
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
