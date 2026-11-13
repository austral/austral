(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** Executes parsed CLI commands. *)
open CliParser
open Error

(** Run a command. Raises [Austral_error] for errors in the command itself;
    reports compile errors and exits. *)
val exec : cmd -> unit

(** Write the error to stderr and to error.html, then exit with a failure
    status. *)
val report_and_exit : error_reporting_mode -> austral_error -> 'a
