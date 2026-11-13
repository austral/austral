(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Austral_core
open Unix
open Error
open HtmlError

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
    let reason =
      match error with
      | Sys_error msg -> Some msg
      | _ -> None
    in
    cli_error (WriteError { path; reason })

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
    cli_error (CCompilerError { command; exit_code = code; stdout; stderr })
  else
    o

let html_error_dump (error: austral_error): unit =
  write_string_to_file "error.html" (render_error_to_html error)
