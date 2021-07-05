open OUnit2
open Austral_core.Common
open Austral_core.Cst
open Austral_core.ParserInterface
open TestUtil

let p = parse_expr

let peq (s: string) (v: 'a) =
  eq v (p s)

let test_parse_nil_constant _ =
  peq "nil" CNilConstant

let test_parse_bool_constant _ =
  peq "true" (CBoolConstant true);
  peq "false" (CBoolConstant false)

let test_parse_int_constant _ =
  peq "0" (CIntConstant "0");
  peq "123" (CIntConstant "123");
  peq "1'000" (CIntConstant "1000")

let test_parse_variable _ =
  peq "a" (CVariable (i "a"));
  peq "A" (CVariable (i "A"));
  peq "Test" (CVariable (i "Test"));
  peq "Test_Test" (CVariable (i "Test_Test"))

let test_parse_funcall_positional _ =
  peq "f(true)" (CFuncall (i "f", ConcretePositionalArgs [CBoolConstant true]));
  peq "f(true, nil)" (CFuncall (i "f", ConcretePositionalArgs [CBoolConstant true; CNilConstant]))

let test_parse_funcall_named _ =
  peq "f(a => true)" (CFuncall (i "f", ConcreteNamedArgs [(i "a", CBoolConstant true)]));
  peq "f(a => true, b => nil)" (CFuncall (i "f", ConcreteNamedArgs [(i "a", CBoolConstant true); (i "b", CNilConstant)]))

let test_parse_arithmetic _ =
  let add a b = CArith (Add, a, b)
  and sub a b = CArith (Subtract, a, b)
  and mul a b = CArith (Multiply, a, b)
  and div a b = CArith (Divide, a, b)
  and v s = CVariable (i s)
  in
  peq "a" (v "a");
  peq "a+b" (add (v "a") (v "b"));
  peq "a-b" (sub (v "a") (v "b"));
  peq "a*b" (mul (v "a") (v "b"));
  peq "a/b" (div (v "a") (v "b"));
  peq "(a-c)*c + x" (add (mul (sub (v "a") (v "c")) (v "c")) (v "x"))

let suite =
  "Expression parser" >::: [
      "Nil constant" >:: test_parse_nil_constant;
      "Bool constant" >:: test_parse_bool_constant;
      "Int constant" >:: test_parse_int_constant;
      "Variable" >:: test_parse_variable;
      "Function call, positional arguments" >:: test_parse_funcall_positional;
      "Function call, named arguments" >:: test_parse_funcall_named;
      "Arithmetic" >:: test_parse_arithmetic
    ]

let _ = run_test_tt_main suite
