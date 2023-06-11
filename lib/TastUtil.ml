(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Type
open Stages.Tast
open Identifier
open StringSet
open Error

module Errors = struct
  let arity_error ~expected ~actual =
    austral_raise GenericError [
      Text "Wrong number of arguments. Expected ";
      Code (string_of_int expected);
      Text " and got ";
      Code (string_of_int actual);
      Text "."
    ]
end

let arglist_size = function
  | (TPositionalArglist l) -> List.length l
  | (TNamedArglist l) -> List.length l

let rec arglist_to_positional (args, names): texpr list =
  let expected = List.length names
  and actual = arglist_size args
  in
  if expected = actual then
    let _ = assert (names_match args names) in
    match args with
    | (TPositionalArglist l) -> l
    | (TNamedArglist l) -> reorder_arglist l names
  else
    Errors.arity_error ~expected ~actual

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
  | TFuncall (_, _, _, ty, _) ->
     ty
  | TMethodCall (_, _, _, _, ty, _) ->
     ty
  | TVarMethodCall { rt; _ } ->
     rt
  | TFptrCall (_, _, rt) ->
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
  | TRefPath (_, _, ty) ->
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
         internal_err ("a dereference expression was constructed whose argument is not a reference type."))
  | TSizeOf _ ->
     Integer (Unsigned, WidthByteSize)

and path_elem_type = function
  | TSlotAccessor (_, t) ->
     t
  | TPointerSlotAccessor (_, t) ->
     t
  | TArrayIndex (_, t) ->
     t

and ref_path_elem_type = function
  | TRefSlotAccessor (_, ty) ->
     ty
