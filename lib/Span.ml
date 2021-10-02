open Lexing

type position = Position of {
      line: int;
      column: int;
    }

type span = Span of {
      filename: string;
      startp: position;
      endp: position;
    }

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
