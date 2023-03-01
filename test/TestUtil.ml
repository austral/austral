(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core.Identifier
open Austral_core.Span

let eq = assert_equal
let i = make_ident

let span l c l' c' =
  Span {
      filename = "";
      startp = Position {
                   line = l;
                   column = c;
                 };
      endp = Position {
                 line = l';
                 column = c';
               };
  }

let simplespan s =
  span 1 0 1 (String.length s)
