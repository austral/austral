(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The linearity errors, and their wording. *)
open Identifier
open Common
open Type
open ErrorText

(** The consumption state of a variable. *)
type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed

let humanize_state (state: var_state): string =
  match state with
  | Unconsumed -> "not yet consumed"
  | BorrowedRead -> "borrowed (read-only)"
  | BorrowedWrite -> "borrowed (read-write)"
  | Consumed -> "consumed"

(** The statements whose branches must leave every variable in the same state. *)
type branching =
  | CaseBranching
  | IfBranching

let branching_words (statement: branching): string =
  match statement with
  | CaseBranching -> "a case"
  | IfBranching -> "an if"

type t =
  | DereferenceNonFree of ty
  | FreeContainsLinear of { name: identifier; universe: universe; parameter: bool }
  | ForgotToConsume of identifier
  | ConsumedInLoop of identifier list
  | InconsistentBranches of { name: identifier; statement: branching; state_a: var_state; state_b: var_state }
  | ConsumedMoreThanOnce of identifier
  | ConsumedWhileBorrowedRead of identifier
  | ConsumedWhileBorrowedWrite of identifier
  | AlreadyConsumed of identifier
  | AssignToUnconsumed of identifier
  | CannotBorrow of { mode: borrow_stmt_kind; name: identifier; state: var_state }
  | NotConsumedAtReturn of identifier
  | AssignToNonFreePath of ty

let render (error: t): err_text =
  match error with
  | DereferenceNonFree ty ->
     [
       Text "Cannot dereference non-free type ";
       Type ty
     ]
  | FreeContainsLinear { name; universe; parameter } ->
     [
       Code (ident_string name);
       Text " was declared to belong to the ";
       Code "Free";
       Text " universe, but it ";
       Text (if parameter then "has a type parameter" else "contains a type");
       Text " that belongs to the ";
       Code (universe_string universe);
       Text " universe."
     ]
  | ForgotToConsume name ->
     [
       Text "Forgot to consume a linear variable: ";
       Code (ident_string name);
       Text "."
     ]
  | ConsumedInLoop names ->
     let c: int = List.length names in
     [
       Text "The ";
       if (c = 1) then
         Text " variable "
       else
         Text " variables ";
       Code (String.concat ", " (List.map ident_string names));
       if (c = 1) then
         Text " was "
       else
         Text " were ";
       Text "consumed in the loop, without afterwards being reassigned."
     ]
  | InconsistentBranches { name; statement; state_a; state_b } ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is used inconsistently in the branches of ";
       Text (branching_words statement);
       Text " statement. In one branch it is ";
       Text (humanize_state state_a);
       Text " while in the other it is ";
       Text (humanize_state state_b);
       Text "."
     ]
  | ConsumedMoreThanOnce name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is consumed multiple times within the same expression.";
     ]
  | ConsumedWhileBorrowedRead name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " cannot be consumed while it is borrowed (immutably).";
     ]
  | ConsumedWhileBorrowedWrite name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text "cannot be consumed while it is borrowed (mutably).";
     ]
  | AlreadyConsumed name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " has already been consumed.";
     ]
  | AssignToUnconsumed name ->
     [
       Text "Cannot assign to the variable ";
       Code (ident_string name);
       Text " because it is not yet consumed."
     ]
  | CannotBorrow { mode; name; state } ->
     let mode_string = match mode with
       | Read -> "borrow (immutably)"
       | Write -> "borrow (mutably)"
       | Reborrow -> "reborrow"
     in
     [
       Text ("Cannot " ^ mode_string ^ " the variable ");
       Code (ident_string name);
       Text " because it is already ";
       Text (humanize_state state);
       Text "."
     ]
  | NotConsumedAtReturn name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " is not consumed by the time of the return statement. Did you forget to call a destructor, or destructure the contents?"
     ]
  | AssignToNonFreePath ty ->
     [
       Text "Cannot assign to a path that ends in a type, ";
       Type ty;
       Text ", not in the Free universe."
     ]
