(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors that have no category yet, and their wording. *)
open Identifier
open Type
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
