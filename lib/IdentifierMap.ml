(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

module IdentifierMap =
  Map.Make(
      struct
        open Identifier
        type t = identifier
        let compare a b = compare (ident_string a) (ident_string b)
      end
    )

module ModuleNameMap =
  Map.Make(
      struct
        open Identifier
        type t = module_name
        let compare a b = compare (mod_name_string a) (mod_name_string b)
      end
    )
