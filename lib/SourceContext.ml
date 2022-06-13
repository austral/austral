open Span

type source_ctx = SourceContext of (int * string) list

let pos_line (Position { line; _}) =
  line

let get_source_ctx (code: string) (span: span): source_ctx =
  let Span { startp; endp; _ } = span in
  (* Split the file into lines. *)
  let lines: string list = String.split_on_char '\n' code in
  (* How many lines of context do we want to show? *)
  let context_line_count: int = 2 in
  (* What's the index of the first line to collect? We use max to ensure it's
     non-negative. *)
  let first_line_idx: int = max ((pos_line startp) - context_line_count - 1) 0 in
  (* What's the index of the last line to collect? We use min to ensure it's not
     greater than the furthest line in the file. *)
  let last_line_idx: int = min ((pos_line endp) + context_line_count - 1) (List.length lines) in
  (* Associate each line with its position. *)
  let lines_with_pos: (int * string) list =
    List.mapi (fun idx elem ->
        (* Get the real line number by adding the index of the first line to the
           index of this line, plus one since line indices begin at zero. *)
        (idx + 1, elem)) lines
  in
  (* Collect the lines. *)
  let collected_lines: (int * string) list =
    List.filteri (fun idx _ -> idx >= first_line_idx && idx <= last_line_idx) lines_with_pos in
  SourceContext collected_lines

let pad (num: int) (width: int): string =
  let s = string_of_int num in
  if (String.length s) > width then
    s
  else
    (String.make (width - (String.length s)) ' ') ^ s

let source_ctx_to_plain_text (ctx: source_ctx): string =
  let SourceContext lines = ctx in
  (* What's the size in characters of the largest line number? *)
  let largest_line_num: int = List.fold_left max 0 (List.map (fun (num, _) -> String.length (string_of_int num)) lines) in
  (* Pad the libe numbers. *)
  let rendered_lines: string list =
    List.map (fun (line_num, line) ->
        (* Pad the line number with zeroes so the line number columns all have
           the same width. *)
        let padded_line_num = pad line_num largest_line_num in
        padded_line_num ^ " | " ^ line)
      lines
  in
  (* Turn the lines into text. *)
  String.concat "\n" rendered_lines
