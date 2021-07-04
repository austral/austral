open Lexing
open Error
(*
let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  "line " ^ l ^ ", column " ^ c
 *)
let position_text s lexbuf =
  let pos = lexbuf.lex_curr_p
  and lines = String.split_on_char '\n' s in
  let line = List.nth lines (pos.pos_lnum - 1) in
  let second_line = String.init (String.length line) (fun i -> if i = (pos.pos_cnum - 1) then '^' else ' ') in
  line ^ "\n" ^ second_line

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try
    f Lexer.token lexbuf
  with Parser.Error ->
    err ("Parse error: " ^ (position_text s lexbuf))

let parse_stmt s =
  parse' Parser.standalone_statement s

let parse_expr s =
  parse' Parser.standalone_expression s
