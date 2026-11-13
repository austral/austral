(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Everything the compiler asks of the operating system: reading and writing
    files, and running the C compiler. *)
open Austral_core
open Error

(** Read the file at the given path into a string. *)
val read_file_to_string : string -> string

(** Write the contents of a string to a file.

    The first argument is the path, the second the contents. *)
val write_string_to_file : string -> string -> unit

(** Represents the output of running an external command. *)
type command_output =
  CommandOutput of { command: string; code: int; stdout: string; stderr: string }

(** Run a shell command, and return its output. *)
val run_command : string -> command_output

(** Call the C compiler on the given file.

    The first argument is the source file path.

    The second argument is the output binary file path. *)
val compile_c_code : string -> string -> command_output

(** Write the HTML page for the error to error.html in the working
    directory. *)
val html_error_dump : austral_error -> unit
