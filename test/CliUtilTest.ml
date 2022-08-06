open OUnit2
open Austral_core.CliUtil

let tests _ =
  assert_equal [] (arglist_to_list (parse_args ["bin"]));
  assert_equal [PositionalArg "foo"] (arglist_to_list (parse_args ["bin"; "foo"]));
  assert_equal [PositionalArg "foo"; BoolFlag "foo"] (arglist_to_list (parse_args ["bin"; "foo"; "--foo"]));
  assert_equal [PositionalArg "foo"; BoolFlag "foo"; ValueFlag ("foo", "bar")] (arglist_to_list (parse_args ["bin"; "foo"; "--foo"; "--foo=bar"]));
  let arglist: arglist = parse_args ["bin"; "--help"; "--foo=bar"; "derp"] in
  assert_equal [BoolFlag "help"; ValueFlag ("foo", "bar"); PositionalArg "derp"] (arglist_to_list arglist);
  assert_equal None (pop_bool_flag arglist "none");
  assert_equal None (pop_value_flag arglist "none");
  assert_equal ["derp"] (get_positional arglist);
  let (arglist, pos) = pop_positional arglist in
  assert_equal ["derp"] pos;
  let arglist: arglist =
    (match (pop_bool_flag arglist "help") with
     | Some arglist -> arglist
     | None -> assert_failure "No --help flag")
  in
  let arglist: arglist =
    (match (pop_value_flag arglist "foo") with
     | Some (arglist, value) ->
        assert_equal "bar" value;
        arglist
     | None -> assert_failure "Wrong")
  in
  assert_equal 0 (arglist_size arglist)

let suite =
  "CliUtil" >::: [
      "tests" >:: tests
    ]

let _ = run_test_tt_main suite
