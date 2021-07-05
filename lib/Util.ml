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
  string_implode (List.filter (fun c' -> c != c') (string_explode s))
