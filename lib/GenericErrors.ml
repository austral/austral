(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors that have no category yet, and their wording. *)
open Identifier
open Type
open TypeParameter
open ErrorText

(** What a name that was not found was expected to name. *)
type name_kind =
  | CallableName
  | VariableName

let name_kind_word (kind: name_kind): string =
  match kind with
  | CallableName -> "callable"
  | VariableName -> "variable"

type t =
  | UnknownName of { kind: name_kind; name: identifier }
  | CannotAssignToConstant
  | CannotAssignToParameter
  | CantAssignToImmutableVar of identifier
  | RefTransformNeedsRef of ty
  | VarCollidesWithDecl of identifier
  | WrongNumberOfArguments of { expected: int; actual: int }
  | BrokenOrphanRule
  | OverlappingInstances
  | DuplicateTypeParameter of type_parameter option
  | TypeWrongArity of { typename: identifier; expected: int; actual: int option }

let render (error: t): err_text =
  match error with
  | UnknownName { kind; name } ->
     [
       Text "I can't find a ";
       Text (name_kind_word kind);
       Text " named ";
       Code (ident_string name);
       Text "."
     ]
  | CannotAssignToConstant ->
     [
       Text "Cannot assign a value to a constant."
     ]
  | CannotAssignToParameter ->
     [
       Text "Cannot assign a value to a function parameter."
     ]
  | CantAssignToImmutableVar name ->
     [
       Text "Cannot assign a value to ";
       Code (ident_string name);
       Text " because it is immutable."
     ]
  | RefTransformNeedsRef ty ->
     [
       Text "The head of a reference transform expression must be of a reference type, but I got ";
       Type ty
     ]
  | VarCollidesWithDecl name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " collides with the name of a declaration."
     ]
  | WrongNumberOfArguments { expected; actual } ->
     [
       Text "Wrong number of arguments. Expected ";
       Code (string_of_int expected);
       Text " and got ";
       Code (string_of_int actual);
       Text "."
     ]
  | BrokenOrphanRule ->
     [
       Text "Orphan rule broken: typeclass and type are both foreign."
     ]
  | OverlappingInstances ->
     [
       Text "Instance overlaps."
     ]
  | DuplicateTypeParameter param ->
     (match param with
      | Some param -> [
          Text "Duplicate type parameter ";
          Code (typaram_name param |> ident_string)
        ]
      | None -> [
          Text "Multiple type parameters have the same name.";
        ])
  | TypeWrongArity { typename; expected; actual } ->
     (let text = [
        Text "The type ";
        Code (ident_string typename);
        Text " expects ";
        Code (string_of_int expected);
        Text " type arguments";
      ]
      in
      text @ match actual with
      | Some actual -> [
          Text ", but I only found ";
          Code (string_of_int actual);
          Text " arguments."
        ]
      | None -> [Text "."])
