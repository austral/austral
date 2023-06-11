(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Type
open Error

type var_source =
  | VarConstant
  | VarParam
  | VarLocal of mutability

type lexenv_inner = LexEnv of (identifier * ty * var_source) list

type lexenv = lexenv_inner ref

let empty_lexenv _ =
  ref (LexEnv [])

let get_var (l: lexenv) (name: identifier): (ty * var_source) option =
  let (LexEnv inner) = !l in
  Option.map (fun (_, t, s) -> (t, s)) (List.find_opt (fun (n, _, _) -> n = name) inner)

let push_var l name ty source =
  let (LexEnv inner) = !l in
  match get_var l name with
  | (Some _) ->
     err ("push_var: var with the name " ^ (ident_string name) ^ " already exists")
  | None ->
     let inner' = LexEnv ((name, ty, source) :: inner) in
     l := inner';
     l

let rec push_vars env l =
  match l with
  | (n,t,s)::rest ->
     push_vars (push_var env n t s) rest
  | [] ->
     env

let remove_var (l: lexenv) (name: identifier): unit =
  let (LexEnv inner) = !l in
  let filtered =
    List.filter (fun (n, _, _) -> not (equal_identifier name n)) inner
  in
  l := LexEnv filtered;
  ()

let rec remove_vars l names =
  match names with
  | [] ->
     ()
  | first::rest ->
     let _ = remove_var l first in
     remove_vars l rest
