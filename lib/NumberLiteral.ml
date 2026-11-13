(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type radix =
  | Decimal
  | Hexadecimal
  | Binary
  | Octal
  | Character
[@@deriving show]

type int_literal = {
  written: string;
  radix: radix;
  value: Z.t [@printer Z.pp_print];
}
[@@deriving show]

type float_literal = {
  written: string;
  fraction_digits: int;
  has_exponent: bool;
}
[@@deriving show]

let without_separators (written: string): string =
  Util.remove_char written '\''

(* Issues #121 and #85: the digits after a `#x`, `#b` or `#o` prefix are read
   through OCaml's 63-bit `int`. A literal of 2^62 or more silently becomes
   another value (#121), and one with too many digits raises
   `Failure "int_of_string"` (#85). Tests pin both. Reading the digits with
   `Z.of_string` here fixes both. *)
let value_through_native_int (ocaml_prefix: string) (written: string): Z.t =
  let digits: string = Util.remove_leading (without_separators written) 2 in
  Z.of_int (int_of_string (ocaml_prefix ^ digits))

let of_decimal (written: string): int_literal =
  (* The lexer admits a sign, digits and separators, all of which `Z.of_string`
     reads once the separators are gone, so this cannot fail. *)
  { written; radix = Decimal; value = Z.of_string (without_separators written) }

let of_hexadecimal (written: string): int_literal =
  { written; radix = Hexadecimal; value = value_through_native_int "0x" written }

let of_binary (written: string): int_literal =
  { written; radix = Binary; value = value_through_native_int "0b" written }

let of_octal (written: string): int_literal =
  { written; radix = Octal; value = value_through_native_int "0o" written }

let of_character (written: string): int_literal =
  (* The quotes around the character are the same character as the digit
     separator. *)
  { written; radix = Character; value = Z.of_int (Util.parse_ascii_char (without_separators written)) }

let of_float (written: string): float_literal =
  let length: int = String.length written in
  let exponent_start: int =
    match (String.index_opt written 'e', String.index_opt written 'E') with
    | (Some i, _) | (None, Some i) -> i
    | (None, None) -> length
  in
  let period: int = String.index written '.' in
  let fraction: string = String.sub written (period + 1) (exponent_start - period - 1) in
  let count_digit (n: int) (c: char): int =
    if '0' <= c && c <= '9' then n + 1 else n
  in
  { written; fraction_digits = String.fold_left count_digit 0 fraction; has_exponent = exponent_start < length }

let of_value (value: Z.t): int_literal =
  { written = Z.to_string value; radix = Decimal; value }

let int_text (literal: int_literal): string =
  match literal.radix with
  | Decimal ->
     (* Issue #88: a decimal literal goes into the C as it was written, so C
        reads one with a leading zero as octal. A test pins it. Printing
        `literal.value` here fixes it. *)
     without_separators literal.written
  | Hexadecimal | Binary | Octal | Character ->
     Z.to_string literal.value

let float_text (literal: float_literal): string =
  without_separators literal.written
