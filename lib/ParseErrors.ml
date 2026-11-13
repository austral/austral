(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The parse errors, and their wording. *)
open ErrorText

type t =
  | FunctionPointerNoArgs
  | LongCharacter of string
  | SyntaxError
  | InvalidCharacter of string
  | StringInvalidCharacter of string
  | StringEof
  | TripleStringEof

let render (error: t): err_text =
  match error with
  | FunctionPointerNoArgs ->
     [
       Text "Function pointer type specifier must have at least one argument."
     ]
  | LongCharacter lit ->
     [
       Text "Character literal '";
       Code lit;
       Text " is more than one character."
     ]
  | SyntaxError ->
     [
       Text "Error during parse."
     ]
  | InvalidCharacter lexeme ->
     [
       Text "Character not allowed in source text: ";
       Code lexeme
     ]
  | StringInvalidCharacter lexeme ->
     [
       Text "Character not allowed in string literal: ";
       Code lexeme
     ]
  | StringEof ->
     [
       Text "End of file in string literal."
     ]
  | TripleStringEof ->
     [
       Text "End of file in triple string literal."
     ]
