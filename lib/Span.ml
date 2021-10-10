open Lexing

type position = Position of {
      line: int;
      column: int;
    }

let pos_line (Position { line; _}) =
  line

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
  "line " ^ (string_of_int (line + 1)) ^ ", column " ^ (string_of_int column)

let span_to_string (Span { filename; startp; endp; }): string =
  "Filename: '" ^ filename ^ "'\nFrom: " ^ (position_to_string startp) ^ "\nTo: " ^ (position_to_string endp)

let pad (num: int) (width: int): string =
  let s = string_of_int num in
  if (String.length s) > width then
    s
  else
    (String.make (width - (String.length s)) ' ') ^ s

let span_text (code: string) (Span { startp; endp; _}): string =
  (* Split the file into lines. *)
  let lines: string list = String.split_on_char '\n' code in
  (* How many lines of context do we want to show? *)
  let context_line_count: int = 2 in
  (* What's the index of the first line to collect? We use max to ensure it's
     non-negative. *)
  let first_line_idx: int = max ((pos_line startp) - context_line_count) 0 in
  (* What's the index of the last line to collect? We use min to ensure it's not
     greater than the furthest line in the file. *)
  let last_line_idx: int = min ((pos_line endp) + context_line_count) (List.length lines) in
  (* Collect the lines. *)
  let collected_lines: string list =
    List.filteri (fun idx _ -> idx >= first_line_idx && idx <= last_line_idx) lines in
  (* Get a set of (line number, line text) pairs. *)
  let numbered_lines: (int * string) list =
    List.mapi (fun idx line ->
        (* Get the real line number by adding the index of the first line to the
           index of this line, plus one since line indices begin at zero. *)
        let line_num = first_line_idx + idx + 1 in
        (line_num, line))
      collected_lines
  in
  (* What's the size in characters of the largest line number? *)
  let largest_line_num: int = List.fold_left max 0 (List.map (fun (ln, _) -> (String.length (string_of_int ln))) numbered_lines) in
  (* Render the lines. *)
  let rendered_lines: string list =
    List.map (fun (line_num, line) ->
        (* Pad the line number with zeroes so the line number columns all have
           the same width. *)
        let padded_line_num = pad line_num largest_line_num in
        padded_line_num ^ " | " ^ line)
      numbered_lines
  in
  (* Turn the lines into text. *)
  String.concat "\n" rendered_lines
