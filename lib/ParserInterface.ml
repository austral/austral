open Error

let parse' f s =
  try
    f Lexer.token (Lexing.from_string s)
  with AustralParseError (p1, p2) ->
    err ("Parse error: '" ^ (String.sub s p1 (p2 - p1)) ^ "'")

let parse_stmt s =
  parse' Parser.statement s

let parse_expr s =
  parse' Parser.expression s
