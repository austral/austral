(* A string where escape characters have been processed into the characters they
   represent. *)
type escaped_string
[@@deriving (show, sexp)]

(* Given a string with escape characters, process the escape characters and
   return a escaped string. *)
val escape_string : string -> escaped_string

(* Return a escaped string as a regular string *)
val escaped_to_string : escaped_string -> string

(* Replace unrepresentable characters in a escaped string (e.g. newlines) with
   escape characters in C/C++ format. *)
val unescape_string : escaped_string -> string
