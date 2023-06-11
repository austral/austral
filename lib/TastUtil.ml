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
  | TTemporary (_, ty) ->
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
  | TSlotAccessor (_, _, ty) -> ty
  | TPointerSlotAccessor (_, _, ty) -> ty
  | TArrayIndex (_, _, ty) -> ty

let rec dump_stmt (stmt: tstmt): string =
   pp stmt 0

and pp (stmt: tstmt) (depth: int): string =
  let indent (s: string): string =
    (String.make depth ' ') ^ s

  and inc: int = depth + 4
  in
  match stmt with
  | TSkip _ ->
     indent "skip\n"
  | TLet (_, _, _, _, body) ->
     (indent "let\n")
     ^ (pp body inc)
     ^ (indent "end let\n")
  | TDestructure (_, _, _, _, body) ->
     (indent "let destructure\n")
     ^ (pp body inc)
     ^ (indent "end let destructure\n")
  | TAssign (_, _, _) ->
     indent "assign\n"
  | TAssignVar _ ->
     indent "assign var\n"
  | TInitialAssign _ ->
     indent "initial assign\n"
  | TIf (_, _, t, f) ->
     (indent "if\n")
     ^ (pp t inc)
     ^ (indent "else\n")
     ^ (pp f inc)
     ^ (indent "end if\n")
  | TCase _ ->
     indent "case\n"
  | TWhile (_, _, body) ->
     (indent "while\n")
     ^ (pp body inc)
     ^ (indent "end while\n")
  | TFor (_, _, _, _, body) ->
     (indent "for\n")
     ^ (pp body inc)
     ^ (indent "end for\n")
  | TBorrow { body; _ } ->
     (indent "borrow\n")
     ^ (pp body inc)
     ^ (indent "end borrow\n")
  | TBlock (_, a, b) ->
     (indent "block\n")
     ^ (pp a inc)
     ^ (pp b inc)
     ^ (indent "end block\n")
  | TDiscarding _ ->
     indent "discarding\n"
  | TReturn _ ->
     indent "return\n"
  | TLetTmp _ ->
     indent "let tmp\n"
  | TAssignTmp _ ->
     indent "assign tmp\n"
