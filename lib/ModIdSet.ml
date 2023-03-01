(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module ModIdSet =
  Set.Make(
      struct
        open Id
        type t = mod_id
        let compare (a: t) (b: t): int =
          let ModId a' = a
          and ModId b' = b in
          compare a' b'
      end
    )
