open OUnit2

let suite =
  "Austral Tests" >::: [
      "Expressions" >:: ExpressionParserTest.suite
    ]

let () =
  run_test_tt_main suite
