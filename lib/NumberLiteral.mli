(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Number literals. The parser reads the text of a literal once, into one of
    these records, and every later stage carries the record. *)

(** How an integer literal was written. *)
type radix =
  | Decimal
  | Hexadecimal
  | Binary
  | Octal
  | Character
[@@deriving show]

type int_literal = {
  written: string;
  (** The text as the lexer saw it, with its prefix, sign and digit
      separators. *)
  radix: radix;
  value: Z.t;
}
[@@deriving show]

type float_literal = {
  written: string;
  (** The text as the lexer saw it, with its sign and digit separators. *)
  fraction_digits: int;
  (** How many digits follow the period. *)
  has_exponent: bool;
}
[@@deriving show]

(** {1 Reading a token}

    One function per token kind. The argument is the token's text. *)

val of_decimal : string -> int_literal

val of_hexadecimal : string -> int_literal

val of_binary : string -> int_literal

val of_octal : string -> int_literal

(** The second argument is the character's code. The parser works it out,
    because a character token that is not one character is a parse error. *)
val of_character : string -> int -> int_literal

val of_float : string -> float_literal

(** An integer literal that nobody wrote: the result of folding, or the zero
    that a unary minus subtracts from. Its text is the value in decimal. *)
val of_value : Z.t -> int_literal

(** {1 Printing} *)

(** The text the compiler prints for an integer literal: into the generated C,
    and into the message that says the literal does not fit a type. *)
val int_text : int_literal -> string

(** The text the compiler prints for a float literal into the generated C. *)
val float_text : float_literal -> string
