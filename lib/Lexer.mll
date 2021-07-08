{
open Lexing
open Error
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'

let string_acc: Buffer.t = Buffer.create 64
}

(* Helper regexes *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)
let whitespace = [' ' '\t']+

let newline = "\r\n" | '\n'

let exponent = 'e' | 'E'
let sign = '+' | '-'
let period = '.'

let comment = "--" [^ '\r' '\n']* (newline)

(* Token regexes *)

let identifier = (alpha) ('_'|alphanum)*
let int_constant = digit (digit|'\'')*
let float_constant = int_constant period int_constant? (exponent sign? int_constant)?

(* Rules *)

rule token = parse
  (* Comments *)
  | comment { token lexbuf }
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  (* Arithmetic operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  (* Comparison operators *)
  | "=" { EQ }
  | "/=" { NEQ }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
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
  | "interface" { TYPECLASS }
  | "implementation" { INSTANCE }
  | "method" { METHOD }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "while" { WHILE }
  | "for" { FOR }
  | "do" { DO }
  | "from" { FROM }
  | "to" { TO }
  | "return" { RETURN }
  | "skip" { SKIP }
  | "Free" { UNIVERSE_FREE }
  | "Linear" { UNIVERSE_LINEAR }
  | "Type" { UNIVERSE_TYPE }
  | "Region" { UNIVERSE_REGION }
  | "pragma" { PRAGMA }
  (* Symbols *)
  | ";" { SEMI }
  | "," { COMMA }
  | "." { PERIOD }
  | ":" { COLON }
  | "=>" { RIGHT_ARROW }
  | ":=" { ASSIGN }
  (* Strings and docstrings *)
  | "```" { DOCSTRING_MARKER }
  (* Identifiers and constants *)
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  | float_constant { FLOAT_CONSTANT (Lexing.lexeme lexbuf) }
  | int_constant { INT_CONSTANT (Lexing.lexeme lexbuf) }
  | '"' { read_string lexbuf }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline { advance_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ {err ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'") }

and read_string = parse
  | '"' { let c = Buffer.contents string_acc in Buffer.clear string_acc; STRING_CONSTANT c }
  | [^ '"'] { Buffer.add_string string_acc (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof { err "End of file in string literal" }
  | _ {err ("Character not allowed in string literal: '" ^ Lexing.lexeme lexbuf ^ "'") }
