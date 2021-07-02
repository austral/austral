{
open Lexing
open Error
open Tokens
}

(* Helper regexes *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)
let whitespace = [' ' '\t' '\r']+

let exponent = 'e' | 'E'
let sign = '+' | '-'
let period = '.'

(* Token regexes *)

let identifier = (alpha) ('_' alpha+)*
let int_constant = digit (digit|'\'')+
let float_constant = int_constant period int_constant (exponent sign int_constant)?

(* Rules *)

rule read_token = parse
  (* Identifiers and constants *)
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  | int_constant { INT_CONSTANT (Lexing.lexeme lexbuf) }
  | float_constant { FLOAT_CONSTANT (Lexing.lexeme lexbuf) }
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
  | "immport" { IMPORT }
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
  | "interface" { INTERFACE }
  | "implementation" { IMPLEMENTATION }
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
  (* Symbols *)
  | ";" { SEMI }
  | "," { COMMA }
  | ":" { COLON }
  | "=>" { RIGHT_ARROW }
  (* etc. *)
  | eof { EOF }
  | _ {err ("Character not allowed in source text: " ^ Lexing.lexeme lexbuf) }