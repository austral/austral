let parse_expr s =
  let lexbuf = Lexing.from_string s in
  Parser.expression Lexer.token lexbuf
