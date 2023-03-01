(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module IdentifierSet =
  Set.Make(
      struct
        open Identifier
        type t = identifier
        let compare a b = compare (ident_string a) (ident_string b)
      end
    )

module SIdentSet =
  Set.Make(
      struct
        open Identifier
        type t = sident
        let compare a b = compare (show_sident a) (show_sident b)
      end
    )
