(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module DeclIdSet =
  Set.Make(
      struct
        open Id
        type t = decl_id
        let compare (a: decl_id) (b: decl_id): int =
          let DeclId a' = a
          and DeclId b' = b in
          compare a' b'
      end
    )
