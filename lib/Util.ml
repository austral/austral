open Identifier
open Error

let string_explode s =
  List.init (String.length s) (String.get s)

let string_implode l =
  String.init (List.length l) (List.nth l)

let read_file_to_string path =
  let stream = open_in path in
  try
    let len = in_channel_length stream in
    let str = really_input_string stream len in
    close_in stream;
    str
  with _ ->
    close_in_noerr stream;
    err ("Failed to read file: " ^ path)

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
