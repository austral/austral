type token =
  (* Identifiers and constants *)
  | IDENTIFIER of string
  | NIL
  | TRUE
  | FALSE
  | INT_CONSTANT of string
  | FLOAT_CONSTANT of string
  (* Brackets *)
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  (* Arithmetic operators *)
  | PLUS
  | MINUS
  | MUL
  | DIV
  (* Comparison operators *)
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  (* Keywords *)
  | MODULE
  | IS
  | BODY
  | IMPORT
  | END
  | CONSTANT
  | TYPE
  | FUNCTION
  | GENERIC
  | RECORD
  | UNION
  | CASE
  | OF
  | WHEN
  | INTERFACE
  | IMPLEMENTATION
  | METHOD
  | IF
  | THEN
  | ELSE
  | LET
  | WHILE
  | FOR
  | DO
  | FROM
  | TO
  | RETURN
  | SKIP
  (* Symbols *)
  | SEMI
  | COMMA
  | COLON
  | RIGHT_ARROW
  (* etc. *)
  | EOF
