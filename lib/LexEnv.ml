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

type lexenv = (identifier * ty * var_source) list

let empty_lexenv =
  []

let get_var (l: lexenv) (name: identifier): (ty * var_source) option =
  Option.map (fun (_, t, s) -> (t, s)) (List.find_opt (fun (n, _, _) -> n = name) l)

let push_var l name ty source =
  match get_var l name with
  | (Some _) ->
     err "push_var: var with this name already exists"
  | None ->
     (name, ty, source) :: l

let rec push_vars env l =
  match l with
  | (n,t,s)::rest ->
     push_vars (push_var env n t s) rest
  | [] ->
     env
