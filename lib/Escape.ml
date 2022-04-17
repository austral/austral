open Util
open Error

type escaped_string = EscapedString of string
[@@deriving show]

let rec escape_string s =
  EscapedString (string_implode (escape_list (string_explode s)))

and escape_list lst =
  match lst with
  | ('\\' :: 'n' :: rest) -> '\n' :: (escape_list rest)
  | ('\\' :: 'r' :: rest) -> '\r' :: (escape_list rest)
  | ('\\' :: 't' :: rest) -> '\t' :: (escape_list rest)
  | ('\\' :: '\'' :: rest) -> '\'' :: (escape_list rest)
  | ('\\' :: '\\' :: rest) -> '\\' :: (escape_list rest)
  | ('\\' :: ' ' :: rest) -> consume_whitespace (' ' :: rest)
  | ('\\' :: '\n' :: rest) -> consume_whitespace (' ' :: rest)
  | ('\\' :: '\r' :: rest) -> consume_whitespace (' ' :: rest)
  | ('\\' :: '\t' :: rest) -> consume_whitespace (' ' :: rest)
  | (head :: rest) -> head :: (escape_list rest)
  | nil -> nil

and consume_whitespace lst =
  match lst with
  | (' '  :: rest) -> consume_whitespace rest
  | ('\n' :: rest) -> consume_whitespace rest
  | ('\r' :: rest) -> consume_whitespace rest
  | ('\t' :: rest) -> consume_whitespace rest
  | ('\\' :: rest) -> rest
  | _ -> err "Bad whitespace escape sequence"

let escaped_to_string (EscapedString s) =
  s

let rec unescape_string (EscapedString s) =
  String.concat "" (List.map unescape_char (string_explode s))

and unescape_char = function
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\'' -> "\\\'"
  | '\\' -> "\\\\"
  | c -> String.make 1 c
