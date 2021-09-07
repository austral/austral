open Identifier
open Error

let string_explode (s: string): char list =
  List.init (String.length s) (String.get s)

let string_implode (l: char list): string =
  String.init (List.length l) (List.nth l)

let read_file_to_string (path: string): string =
  let stream = open_in path in
  try
    let len = in_channel_length stream in
    let str = really_input_string stream len in
    close_in stream;
    str
  with _ ->
    close_in_noerr stream;
    err ("Failed to read file: " ^ path)

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

let ident_set_eq a b =
  let sorter a b = compare (ident_string a) (ident_string b) in
  (List.sort sorter a) = (List.sort sorter b)
