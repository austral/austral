open OUnit2
open Austral_core.Util
open TestUtil

let test_run_command _ =
  let (CommandOutput { command; code; stdout; stderr }) = run_command "echo derp" in
  eq "echo derp" command;
  eq 0 code;
  eq "derp" stdout;
  eq "" stderr

let suite =
  "Utilities" >::: [
      "run_command" >:: test_run_command
    ]

let _ = run_test_tt_main suite
