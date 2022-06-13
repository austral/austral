open Lexing

type position = Position of {
      (* Lines range from 1 to infinity. *)
      line: int;
      (* Columns range from 0 to infinity. *)
      column: int;
    }
[@@deriving (show, sexp)]

type span = Span of {
      filename: string;
      startp: position;
      endp: position;
    }
[@@deriving (show, sexp)]

val from_lexbuf : lexbuf -> span

(* Menhir has a special token, $loc, that evaluates to a (pos, pos) token. This
   function takes that pair and returns a span. *)
val from_loc : (Lexing.position * Lexing.position) -> span

val position_to_string : position -> string

val span_to_string : span -> string

(* Given a string containing a code file, and a span, return the relevant lines
   plus some context. *)
val span_text : string -> span -> string

val empty_span : span
