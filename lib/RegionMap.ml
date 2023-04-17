(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open IdentifierMap
open Error
open Region

type region_map = region IdentifierMap.t

let empty_region_map = IdentifierMap.empty

let get_region m n =
  IdentifierMap.find_opt n m

let add_region m n r =
  match get_region m n with
  | Some _ ->
     err "Region map already has a region with this name"
  | None ->
     IdentifierMap.add n r m
