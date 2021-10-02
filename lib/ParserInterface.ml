open Lexing
open Cst
open Error

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let position_text s lexbuf =
  let pos = lexbuf.lex_curr_p
  and lines = String.split_on_char '\n' s
  and prefix = "    " in
  let current_line_prefix = "    " ^ (string_of_int pos.pos_lnum) ^ " |"
  in
  let second_line_prefix = String.make (String.length current_line_prefix) ' ' in
  let previous_line = if pos.pos_lnum = 1 then
                        None
                      else
                        Some ((string_of_int (pos.pos_lnum - 1)) ^ " |" ^ (List.nth lines (pos.pos_lnum - 2)))
  and nextline = if pos.pos_lnum = (List.length lines) then
                   None
                 else
                   Some ((string_of_int (pos.pos_lnum + 1)) ^ " |" ^ (List.nth lines (pos.pos_lnum)))
  and current_line = current_line_prefix ^ List.nth lines (pos.pos_lnum - 1) in
  let second_line = String.init (String.length current_line) (fun i -> if i = (colnum pos) then '^' else ' ') in
  let previous_str =
    (match previous_line with
     | Some s -> prefix ^ s ^ "\n"
     | None -> "")
    and nextline_str =
      (match nextline with
       | Some s -> prefix ^ s ^ "\n"
       | None -> "")
  in
  previous_str ^ current_line ^ "\n" ^ second_line_prefix ^ second_line ^ "\n" ^ nextline_str

let parse' f s _ =
  let lexbuf = Lexing.from_string s in
  try
    f Lexer.token lexbuf
  with Parser.Error ->
        err ("Parse error (" ^ (pos_string lexbuf.lex_curr_p) ^ "): \n" ^ (position_text s lexbuf))
     | Programmer_error msg ->
        err ("Parse error (" ^ (pos_string lexbuf.lex_curr_p) ^ "): \n" ^ msg ^ "\n" ^ (position_text s lexbuf))

let parse_module_int (s:string) (filename: string): concrete_module_interface  =
  parse' Parser.module_int s filename

let parse_module_body (s: string) (filename: string): concrete_module_body =
  parse' Parser.module_body s filename

let parse_stmt s =
  parse' Parser.standalone_statement s ""

let parse_expr s =
  parse' Parser.standalone_expression s ""
