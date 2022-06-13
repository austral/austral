open Lexing
open Sexplib
open Std

type position = Position of {
      line: int;
      column: int;
    }
[@@deriving (show, sexp)]

type span = Span of {
      filename: string;
      startp: position;
      endp: position;
    }
[@@deriving (show, sexp)]

let from_lexbuf (lexbuf: lexbuf): span =
  let start_pos = lexbuf.lex_start_p
  and end_pos = lexbuf.lex_curr_p in
  Span {
      filename = start_pos.pos_fname;
      startp = Position {
                  line = start_pos.pos_lnum;
                  column = start_pos.pos_cnum - start_pos.pos_bol;
                };
      endp = Position {
                line = end_pos.pos_lnum;
                column = end_pos.pos_cnum - end_pos.pos_bol;
              };
    }

let from_loc (start_pos, end_pos): span =
    Span {
      filename = start_pos.pos_fname;
      startp = Position {
                  line = start_pos.pos_lnum;
                  column = start_pos.pos_cnum - start_pos.pos_bol;
                };
      endp = Position {
                line = end_pos.pos_lnum;
                column = end_pos.pos_cnum - end_pos.pos_bol;
              };
    }

let position_to_string (Position { line; column }): string =
  "line " ^ (string_of_int line) ^ ", column " ^ (string_of_int column)

let span_to_string (Span { filename; startp; endp; }): string =
  "Filename: '" ^ filename ^ "'\nFrom: " ^ (position_to_string startp) ^ "\nTo: " ^ (position_to_string endp)

let empty_span: span = Span {
                           filename = "";
                           startp = Position {
                                        line = 0;
                                        column = 0;
                                      };
                           endp = Position {
                                      line = 0;
                                      column = 0;
                                    };
                         }
