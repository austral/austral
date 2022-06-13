open Cst
open Span
open Error
open ErrorText
open Reporter

let raise_parse_error (span: span): 'a =
  adorn_error_with_span span (fun _ -> austral_raise "Parse Error" [Text "Error during parse."])

let parse' f (code: string) (filename: string) =
  let lexbuf = Lexing.from_string code in
  Lexing.set_filename lexbuf filename;
  try
    f Lexer.token lexbuf
  with Parser.Error ->
        raise_parse_error (from_lexbuf lexbuf)
     | Austral_error error ->
        raise (Austral_error error)

let parse_module_int (s:string) (filename: string): concrete_module_interface  =
  with_frame "Parse module interface"
    (fun _ ->
      ps ("Filename", filename);
      parse' Parser.module_int s filename)

let parse_module_body (s: string) (filename: string): concrete_module_body =
    with_frame "Parse module body"
      (fun _ ->
        ps ("Filename", filename);
        parse' Parser.module_body s filename)

let parse_stmt s =
  parse' Parser.standalone_statement s ""

let parse_expr s =
  parse' Parser.standalone_expression s ""
