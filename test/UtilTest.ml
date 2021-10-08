open OUnit2
open Austral_core.Util
open TestUtil

let test_run_command _ =
  let (CommandOutput { command; code; stdout; stderr }) = run_command "echo derp" in
  eq "echo derp" command;
  eq 0 code;
  eq "derp" stdout;
  eq "" stderr

let test_search_replace _ =
  let out = search_replace {
                text = "The quick brown fox.";
                search = "quick";
                replacement = "slow"
              }
  in
  print_endline out;
  assert_equal out "The slow brown fox."

let suite =
  "Utilities" >::: [
      "run_command" >:: test_run_command;
      "search_replace" >:: test_search_replace
    ]

let _ = run_test_tt_main suite
