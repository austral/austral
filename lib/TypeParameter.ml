(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Type
open Sexplib
open Std

type type_parameter = TypeParameter of identifier * universe * qident * sident list
[@@deriving (show, sexp)]

let make_typaram (i, u, q, s) =
  TypeParameter (i, u, q, s)

let typaram_name (TypeParameter (i, _, _, _)) =
  i

let typaram_universe (TypeParameter (_, u, _, _)) =
  u

let typaram_source (TypeParameter (_, _, s, _)) =
  s

let typaram_constraints (TypeParameter (_, _, _, cs)) =
  cs

let typaram_to_tyvar (typaram: type_parameter): type_var =
  let (TypeParameter (n, u, f, cs)) = typaram in
  TypeVariable (n, u, f, cs)

let tyvar_to_typaram (tyvar: type_var): type_parameter =
  let (TypeVariable (name, universe, source, constraints)) = tyvar in
  TypeParameter (name, universe, source, constraints)
