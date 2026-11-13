(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier

type compiler

(** The compiler after compiling the built-in modules. Raises [Austral_error]
    if a built-in module does not compile. *)
val empty_compiler : unit -> compiler

val compiler_code : compiler -> string

type module_source =
  | TwoFileModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }
  | BodyModuleSource of {
      body_filename: string;
      body_code: string
    }

val compile_multiple : compiler -> module_source list -> compiler

val compile_entrypoint : compiler -> module_name -> identifier -> compiler

val post_compile : compiler -> compiler
