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
  assert_equal out "The slow brown fox."

let test_map_with_context _ =
  let lst = ["a"; "b"; "c"; "d"]
  and ctx = 0
  and f ((c, a): (int * string)): (int * string) =
    (c + 1, String.uppercase_ascii a)
  in
  let (ctx', results) = map_with_context f ctx lst in
  assert_equal ctx' 4;
  assert_equal results ["A"; "B"; "C"; "D"]

let suite =
  "Utilities" >::: [
      "run_command" >:: test_run_command;
      "search_replace" >:: test_search_replace;
      "map_with_context" >:: test_map_with_context
    ]

let _ = run_test_tt_main suite
