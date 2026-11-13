(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors with the definition of the program entrypoint, and their wording. *)
open Identifier
open Type
open ErrorText

type t =
  | MultipleParameters
  | NoSuchFunction of { name: identifier; module_name: module_name }
  | NotFunction
  | NotMonomorphic
  | NotPublic
  | WrongParameterType of ty
  | WrongReturnType of ty

let render (error: t): err_text =
  match error with
  | MultipleParameters ->
     [
       Text "The entrypoint function should take a single parameter with type ";
       Code "RootCapability";
       Text ", or none at all, but it has several."
     ]
  | NoSuchFunction { name; module_name } ->
     [
       Text "No function named ";
       Code (ident_string name);
       Text " in module ";
       Code (mod_name_string module_name)
     ]
  | NotFunction ->
     [
       Text "The entrypoint must be a function."
     ]
  | NotMonomorphic ->
     [
       Text "The entrypoint function cannot be generic."
     ]
  | NotPublic ->
     [
       Text "The entrypoint function is not public.";
       Break;
       Text "Consider declaring it in the module interface."
     ]
  | WrongParameterType ty ->
     [
       Text "The type of the entrypoint argument must be ";
       Code "RootCapability";
       Text " but it is ";
       Type ty
     ]
  | WrongReturnType ty ->
     [
       Text "The type of the entrypoint function must be ";
       Code "ExitCode";
       Text " but it is ";
       Type ty
     ]
