(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Cst
open Span
open Error

let parse' f (code: string) (filename: string) =
  let lexbuf = Lexing.from_string code in
  Lexing.set_filename lexbuf filename;
  try
    f Lexer.token lexbuf
  with Parser.Error ->
        parse_error_at (from_lexbuf lexbuf) SyntaxError
     | Austral_error error ->
        raise (Austral_error error)

let parse_module_int (s:string) (filename: string): concrete_module_interface  =
  parse' Parser.module_int s filename

let parse_module_body (s: string) (filename: string): concrete_module_body =
    parse' Parser.module_body s filename

let parse_expr s =
  parse' Parser.standalone_expression s ""
