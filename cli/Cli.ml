(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Austral_core
open CliUtil
open CliParser
open CliEngine
open Error

let rec main (args: string list): unit =
  try
    main' args;
    exit 0
  with Austral_error error ->
    report_and_exit ErrorReportPlain error

and main' (args: string list): unit =
  let arglist: arglist = parse_args args in
  let cmd: cmd = parse arglist in
  exec cmd
