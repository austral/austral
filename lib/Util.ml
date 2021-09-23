open Identifier
open Error

let string_explode (s: string): char list =
  List.init (String.length s) (String.get s)

let string_implode (l: char list): string =
  String.init (List.length l) (List.nth l)

let read_file_to_string (path: string): string =
  let rec read_stream stream =
    try
      let line = input_line stream in
      line :: (read_stream stream)
    with End_of_file ->
      []
  in
  let stream = open_in path in
  String.concat "\n" (read_stream stream)

let write_string_to_file (path: string) (contents: string): unit =
  let stream = open_out path in
  try
    Printf.fprintf stream "%s" contents;
    close_out stream;
    ()
  with _ ->
    close_out_noerr stream;
    err ("Failed to write to file: " ^ path)

let remove_char (s: string) (c: char)  =
  string_implode (List.filter (fun c' -> c <> c') (string_explode s))

let replace_char (s: string) (c: char) (r: string): string =
  let replace char =
    if char = c then
      string_explode r
    else
      [char]
  in
  string_implode (List.concat (List.map replace (string_explode s)))

let remove_leading (s: string) (n: int): string =
  String.sub s n ((String.length s) - n)

let parse_hex (s: string): int =
  int_of_string ("0x" ^ s)

let parse_bin (s: string): int =
  int_of_string ("0b" ^ s)

let parse_oct (s: string): int =
  int_of_string ("0o" ^ s)

let parse_ascii_char (s: string): int =
  match s with
  | "\\n" ->
     97
  | "\\r" ->
     13
  | "\\t" ->
     9
  | "\\\\" ->
     92
  | _ ->
     (match (string_explode s) with
      | [c] ->
         Char.code c
      | _ ->
         err "Character literal too long.")

let ident_set_eq a b =
  let sorter a b = compare (ident_string a) (ident_string b) in
  (List.sort sorter a) = (List.sort sorter b)
