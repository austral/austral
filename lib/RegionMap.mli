(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Implements the region map type. *)
open Identifier
open Region

type region_map

val empty_region_map : region_map

val add_region : region_map -> identifier -> region -> region_map

val get_region : region_map -> identifier -> region option
