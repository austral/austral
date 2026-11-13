(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_cli.Os
open TestUtil

let test_run_command _ =
  let (CommandOutput { command; code; stdout; stderr }) = run_command "echo derp" in
  eq "echo derp" command;
  eq 0 code;
  eq "derp" stdout;
  eq "" stderr

let suite =
  "Operating system" >::: [
      "run_command" >:: test_run_command
    ]

let _ = run_test_tt_main suite
