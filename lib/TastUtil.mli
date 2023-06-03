(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Utils for the TAST *)
open Type
open Tast
open Identifier

val arglist_size : typed_arglist -> int

(* This function converts an argument list to a list of typed expressions.

   The second argument is the list of parameter names of the function being
   called.

   If the argument list is a positional argument list, we return list is
   returned unchanged.

   If this is a named argument list, we convert it to a positional list using
   the given list of parameter names.

   For example, if the given list of parameter names is ["a", "b", "c"], and the
   list is a named list with a map like {"b" => 3, "a" => 3.14, "c" =>
   "hello!"}, this returns the list [3.14, 3, "hello!"].

   Checks:

       1. The list of parameter names has the same size as the argument
          list.
       2. If the argument list is a named argument list, then the set of keys
          is exactly the same as the set of given parameter names.
*)
val arglist_to_positional : typed_arglist * identifier list -> texpr list

val get_type : texpr -> ty

val path_type : typed_path_expr -> ty

val path_head : typed_path_expr -> identifier

val path_head_ty : typed_path_expr -> ty
