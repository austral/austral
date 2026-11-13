(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type t = {
    name: string;
    (* Whether the reserved-word escape applies when the name is rendered. *)
    escape: bool;
  }

let generated (name: string): t =
  { name; escape = false }

let austral (name: string): t =
  { name; escape = true }

let external_name (name: string): t =
  { name; escape = false }

let austral_unescaped (name: string): t =
  { name; escape = false }

let external_escaped (name: string): t =
  { name; escape = true }

(* https://en.cppreference.com/w/c/keyword
   The match is not anchored at the end of the name. *)
let starts_with_c_keyword (name: string): bool =
  Str.string_match (Str.regexp "alignas\\|alignof\\|auto\\|bool\\|break\\|case\\|char\\|const\\|constexpr\\|continue\\|default\\|do\\|double\\|else\\|enum\\|extern\\|false\\|float\\|for\\|goto\\|if\\|inline\\|int\\|long\\|nullptr\\|register\\|restrict\\|return\\|short\\|signed\\|sizeof\\|static\\|static_assert\\|struct\\|switch\\|thread_local\\|true\\|typedef\\|typeof\\|typeof_unqual\\|union\\|unsigned\\|void\\|volatile\\|while\\|_Alignas\\|_Alignof\\|_Atomic\\|_BitInt\\|_Bool\\|_Complex\\|_Decimal128\\|_Decimal32\\|_Decimal64\\|_Generic\\|_Imaginary\\|_Noreturn\\|_Static_assert\\|_Thread_local\\|_Pragma\\|asm\\|fortran") name 0

let render (ident: t): string =
  if ident.escape && starts_with_c_keyword ident.name then
    "au_ckw_" ^ ident.name
  else
    ident.name

let equal (a: t) (b: t): bool =
  String.equal (render a) (render b)
