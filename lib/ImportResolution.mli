(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Imports
open Cst
open Env

(* Import resolution means taking a list of imports and resolving them to a map
   of nicknames to qualified identifiers.

   In the process, we need to validate a few things:

       1. Check that imports refer to modules that exist in the module
          environment.

       2. Check that imported names refer to declarations that exist in the
          referenced module.

       3. Check that those declarations are public (in the case of functions) or
          either public or opaque (in the case of types). Otherwise, we can't
          import them.

       4. Check that we don't repeat any names within an import list.

       5. Ensure that imports don't collide with each other: if we have `import
          A (s)` and `import B (s)`, this is clearly an error.

   Note that we don't check that imports don't collide with declarations in the
   module. This is because this pass just focuses on the imports as a standalone
   unit.

   The first argument is the name of the module we're importing into.

*)
val resolve : module_name -> module_kind -> env -> concrete_import_list list -> import_map
