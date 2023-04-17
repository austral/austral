(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Sexplib
open Std

type region = Region of int
[@@deriving (eq, show, sexp)]

let region_id (Region i) = i

let static_region_name = make_ident "Static"

let static_region = Region (0)

let region_counter: int ref = ref 1

let fresh_region _ =
  let i = !region_counter in
  region_counter := i + 1;
  Region i