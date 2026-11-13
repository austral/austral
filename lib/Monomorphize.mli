(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Env
open Stages.Tast
open Stages.Mtast

(** This is the entrypoint to the monomorphization pass.

    Monomorphizing a module involves:

    1. Take all the declarations and monomorphize the ones you can (e.g.:
   concrete function definitions, concrete records, concrete instances).

    2. In the process, monomorphs will be collected into the environment.

    3. Get all uninstantiated monomorphs from the environment, and instantiate
   them. Again, new (uninstantiated) monomorphs will be collected and added to
   the environment: for example, if you instantiate a generic function `f` and
   the body of that function contains a call to a generic function `g` with
   monomorphic arguments, that monomorph of `g` will be added to the
   environment.

    4. Repeat step #3 until there are no more uninstantiated monomorphs in the
   environment.

    5. You're done.

*)
val monomorphize : env -> typed_module -> (env * mono_module)

val monomorphize_wrappers : env -> env * mdecl list
