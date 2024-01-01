(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Unix
open Error

module Errors = struct
  let c_compiler_error ~command ~exit_code ~stdout ~stderr =
    austral_raise CliError [
      Text "Failed to run C compiler.";
      Break;
      Text "Command: ";
      Code command;
      Break;
      Text "Exit code: ";
      Text (string_of_int exit_code);
      Break;
      Text "Standard output:\n";
      Text stdout;
      Break;
      Text "Standard error:\n";
      Text stderr
    ]

  let long_character lit =
    austral_raise ParseError [
      Text "Character literal '";
      Code lit;
      Text " is more than one character."
    ]

  let write_error ~path ~error = match error with
  | Sys_error msg ->
      austral_raise CliError [
        Text "Failed to write to file ";
        Code path;
        Text ": ";
        Break;
        Text msg
      ]
  | _ ->
      austral_raise CliError [
        Text "Failed to write to file ";
        Code path
      ]
end

let string_explode (s: string): char list =
  List.init (String.length s) (String.get s)

let string_implode (l: char list): string =
  String.init (List.length l) (List.nth l)

let read_stream_to_string stream: string =
  let rec read_stream stream =
    try
      let line = input_line stream in
      line :: (read_stream stream)
    with End_of_file ->
      []
  in
  String.trim (String.concat "\n" (read_stream stream))

let read_file_to_string (path: string): string =
  let stream = open_in path in
  let contents = read_stream_to_string stream in
  close_in stream;
  contents

let write_string_to_file (path: string) (contents: string): unit =
  let stream = open_out path in
  try
    Printf.fprintf stream "%s" contents;
    close_out stream;
    ()
  with error ->
    close_out_noerr stream;
    Errors.write_error ~path ~error

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

type replacement = { text: string; search: string; replacement: string }

let search_replace (r: replacement): string =
  let { text: string; search: string; replacement: string } = r in
  let re = Str.regexp_string search in
  Str.global_replace re replacement text

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
     10
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
         Errors.long_character s)

let rec count_leading_whitespace (s: string): int =
  let chars = string_explode s in
  match chars with
  | ' '::rest ->
     1 + (count_leading_whitespace (string_implode rest))
  | _ ->
     0

let min_list (l: int list): int =
  match l with
  | first::rest ->
     List.fold_left min first rest
  | [] ->
     err "empty list passed to min_list"

let trim_common_whitespace (s: string): string =
  let lines = String.split_on_char '\n' s in
  let lines = List.map (fun s -> (count_leading_whitespace s, s)) lines in
  let min_whitespace: int = min_list (List.map (fun (w, _) -> w) lines) in
  let lines: string list = List.map (fun (_, s) -> remove_leading s min_whitespace) lines in
  String.concat "\n" lines

let process_triple_string (s: string): string =
  let lines = String.split_on_char '\n' s in
  match lines with
  | first::rest ->
     let first = if (String.equal (String.trim first) "") then
                   ""
                 else
                   first
     in
     (match (List.rev rest) with
      | last::body ->
         let last = if (String.equal (String.trim last) "") then
                      ""
                    else
                      last
         in
         let s = first ^ (String.concat "\n" (List.rev body)) ^ last in
         trim_common_whitespace s
      | [] ->
         first)
  | [] ->
     err "split_on_char returned empty"

let ident_set_eq a b =
  let sorter a b = compare (ident_string a) (ident_string b) in
  (List.sort sorter a) = (List.sort sorter b)

let get_exit_code = function
  | WEXITED i -> i
  | WSIGNALED i -> i
  | WSTOPPED i -> i

type command_output =
  CommandOutput of { command: string; code: int; stdout: string; stderr: string }

let run_command (command: string): command_output =
  let (stdout_chan, stdin_chan, stderr_chan) = open_process_full command [|"PATH=" ^ (getenv "PATH")|] in
  let stdout = read_stream_to_string stdout_chan
  and stderr = read_stream_to_string stderr_chan in
  let proc_stat = close_process_full (stdout_chan, stdin_chan, stderr_chan) in
  CommandOutput {
      command = command;
      code = get_exit_code proc_stat;
      stdout = stdout;
      stderr = stderr
    }

let compile_c_code (source_path: string) (output_path: string): command_output =
  let cmd = "cc " ^ source_path ^ " -fwrapv -lm -o " ^ output_path in
  let o = run_command cmd in
  let (CommandOutput { command; code; stdout; stderr }) = o in
  if code <> 0 then
    Errors.c_compiler_error ~command ~exit_code:code ~stdout ~stderr
  else
    o

let rec map_with_context (f: ('c * 'a) -> ('c * 'b)) (ctx: 'c) (list: 'a list): ('c * ('b list)) =
  match list with
  | first::rest ->
     let (ctx', b) = f (ctx, first) in
     let (ctx'', rest') = map_with_context f ctx' rest in
     (ctx'', b :: rest')
  | [] ->
     (ctx, [])

let rec iter_with_context (f: ('c -> 'a -> 'c)) (ctx: 'c) (list: 'a list): 'c =
  match list with
  | first::rest ->
     let ctx' = f ctx first in
     iter_with_context f ctx' rest
  | [] ->
     ctx

let last (lst: 'a list): 'a =
  let lst: 'a list = List.rev lst in
  match lst with
  | first::_ ->
     first
  | [] ->
     internal_err "Util.last called with an empty list."

let butlast (lst: 'a list): 'a list =
  let lst: 'a list = List.rev lst in
  match lst with
  | _::rest ->
     rest
  | [] ->
     internal_err "Util.butlast called with an empty list."
