(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Type

type err_text = text_elem list

and text_elem =
  | Text of string
  | Code of string
  | Type of ty
  | Break

let rec error_text_to_plain (txt: err_text): string =
  String.concat "" (List.map text_elem_to_plain txt)

and text_elem_to_plain (elem: text_elem): string =
  match elem with
  | Text s ->
     s
  | Code c ->
     "`" ^ c ^ "`"
  | Type ty ->
     "`" ^ (type_string ty) ^ "`"
  | Break ->
     "\n\n"

let rec error_text_to_html (txt: err_text): string =
    String.concat "" (List.map text_elem_to_html txt)

and text_elem_to_html (elem: text_elem): string =
  match elem with
  | Text s ->
      s
  | Code c ->
      "<code>" ^ c ^ "</code>"
  | Type ty ->
      "<code>" ^ (type_string ty) ^ "</code>"
  | Break ->
      "<br><br>"
