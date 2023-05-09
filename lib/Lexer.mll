(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

{
open Lexing
open Error
open Parser

module Errors = struct
  let raise_err lexbuf kind text : 'a =
    let error =
      AustralError {
        module_name = None;
        kind = kind;
        text = text;
        span = Some (Span.from_lexbuf lexbuf);
        source_ctx = None
      }
    in
    raise (Austral_error error)

  let invalid_character lexbuf =
    raise_err lexbuf ParseError [
      Text "Character not allowed in source text: ";
      Code (Lexing.lexeme lexbuf)
    ]

  let single_string_invalid_character lexbuf =
    raise_err lexbuf ParseError [
      Text "Character not allowed in string literal: ";
      Code (Lexing.lexeme lexbuf)
    ]

  let single_string_eof lexbuf =
    raise_err lexbuf ParseError [
      Text "End of file in string literal."
    ]

  let triple_string_eof lexbuf =
    raise_err lexbuf ParseError [
      Text "End of file in triple string literal."
    ]
end

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'

let string_acc: Buffer.t = Buffer.create 64

let triple_string_acc: Buffer.t = Buffer.create 64
}

(* Helper regexes *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)
let whitespace = [' ' '\t']+
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let bin_digit = ['0'-'1']
let oct_digit = ['0'-'7']

let ascii_char = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '!' '"' '#' '$' '%' '&' ''' ')' '(' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '`' '~' ']' '[' '^' '_' '{' '|' '}']

let newline = "\r\n" | '\n'

let exponent = 'e' | 'E'
let sign = '+' | '-'
let period = '.'

let comment = "--" [^ '\r' '\n']* newline?

(* Token regexes *)

let identifier = (alpha) ('_'|alphanum)*
let dec_constant = sign? digit (digit|'\'')*
let hex_constant = '#' 'x' hex_digit (hex_digit|'\'')*
let bin_constant = '#' 'b' bin_digit (bin_digit|'\'')*
let oct_constant = '#' 'o' oct_digit (oct_digit|'\'')*
let float_constant = sign? dec_constant period dec_constant? (exponent sign? dec_constant)?
let char_constant = '\'' (ascii_char | '\\' ['n' 'r' 't' '\\']) '\''

(* Rules *)

rule token = parse
  (* Comments *)
  | comment { advance_line lexbuf; token lexbuf }
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCURLY }
  | "}" { RCURLY }
  (* Arithmetic operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  (* Comparison operators *)
  | "=" { EQ }
  | "/=" { NEQ }
  | "<=" { LTE }
  | "<" { LT }
  | ">=" { GTE }
  | ">" { GT }
  (* Logical operators *)
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  (* Borrowing *)
  | "&!" { BORROW_WRITE }
  | "&(" { REF_TRANSFORM }
  | "&" { BORROW_READ }
  | "reborrow" { REBORROW }
  (* Keywords *)
  | "module" { MODULE }
  | "is" { IS }
  | "body" { BODY }
  | "import" { IMPORT }
  | "as" { AS }
  | "end" { END }
  | "constant" { CONSTANT }
  | "type" { TYPE }
  | "function" { FUNCTION }
  | "generic" { GENERIC }
  | "record" { RECORD }
  | "union" { UNION }
  | "case" { CASE }
  | "of" { OF }
  | "when" { WHEN }
  | "typeclass" { TYPECLASS }
  | "instance" { INSTANCE }
  | "method" { METHOD }
  | "if" { IF }
  | "else" whitespace+ "if" { ELSE_IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "var" { VAR }
  | "while" { WHILE }
  | "for" { FOR }
  | "do" { DO }
  | "from" { FROM }
  | "to" { TO }
  | "borrow" { BORROW }
  | "borrow!" { MUTABLE_BORROW }
  | "in" { IN }
  | "return" { RETURN }
  | "skip" { SKIP }
  | "Free" { UNIVERSE_FREE }
  | "Linear" { UNIVERSE_LINEAR }
  | "Type" { UNIVERSE_TYPE }
  | "Region" { UNIVERSE_REGION }
  | "pragma" { PRAGMA }
  | "sizeof" { SIZEOF }
  (* Symbols *)
  | ";" { SEMI }
  | "," { COMMA }
  | "." { PERIOD }
  | ":" { COLON }
  | "->" { HYPHEN_RIGHT }
  | "=>" { RIGHT_ARROW }
  | ":=" { ASSIGN }
  | "!" { DEREF }
  (* Strings and docstrings *)
  | "\"\"\"" { read_triple_string lexbuf }
  | '"' { read_string lexbuf }
  (* Identifiers and constants *)
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  | float_constant { FLOAT_CONSTANT (Lexing.lexeme lexbuf) }
  | dec_constant { DEC_CONSTANT (Lexing.lexeme lexbuf) }
  | hex_constant { HEX_CONSTANT (Lexing.lexeme lexbuf) }
  | bin_constant { BIN_CONSTANT (Lexing.lexeme lexbuf) }
  | oct_constant { OCT_CONSTANT (Lexing.lexeme lexbuf) }
  | char_constant { CHAR_CONSTANT (Lexing.lexeme lexbuf) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  (* Specials *)
  | "@embed" { EMBED }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline { advance_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ {
    Errors.invalid_character lexbuf
  }

and read_string = parse
  | '"' { let c = Buffer.contents string_acc in Buffer.clear string_acc; STRING_CONSTANT c }
  | '\\' '"' { Buffer.add_char string_acc '"'; read_string lexbuf }
  | [^ '"'] { Buffer.add_string string_acc (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof { Errors.single_string_eof lexbuf }
  | _ { Errors.single_string_invalid_character lexbuf }

and read_triple_string = parse
  | "\"\"\"" { let c = Buffer.contents triple_string_acc in Buffer.clear triple_string_acc; TRIPLE_STRING_CONSTANT c }
  | '\n' { advance_line lexbuf; Buffer.add_string triple_string_acc "\n"; read_triple_string lexbuf }
  | '\\' "\"\"\"" { Buffer.add_string triple_string_acc "\"\"\""; read_triple_string lexbuf }
  | eof { Errors.triple_string_eof lexbuf }
  | _ { Buffer.add_string triple_string_acc (Lexing.lexeme lexbuf); read_triple_string lexbuf }
