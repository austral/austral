(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier

type region
[@@deriving (eq, show, sexp)]

val region_id: region -> int

val static_region_name : identifier

val static_region : region

val fresh_region : unit -> region
