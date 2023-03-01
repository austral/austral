(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
(*open Austral_core.Common*)
open Austral_core.Cst
open Austral_core.ParserInterface
open TestUtil

let p = parse_expr

let peq (s: string) (v: 'a) =
  eq v (p s)

let test_parse_nil_constant _ =
  let s = "nil" in
  peq s (CNilConstant (simplespan s))

let test_parse_bool_constant _ =
  let s = "true" in
  peq s (CBoolConstant (simplespan s, true));
  let s = "false" in
  peq s (CBoolConstant (simplespan s, false))

let test_parse_int_constant _ =
  let s = "0" in
  peq s (CIntConstant (simplespan s, s));
  let s = "123" in
  peq s (CIntConstant (simplespan s, s));
  let s = "1'000" in
  peq s (CIntConstant (simplespan s, "1000"))

let test_parse_float_constant _ =
  let s = "0.0" in
  peq s (CFloatConstant (simplespan s, s));
  let s = "123.3" in
  peq s (CFloatConstant (simplespan s, s));
  let s = "1'000.0e12" in
  peq s (CFloatConstant (simplespan s, "1000.0e12"));
  let s = "1'000.000'1e-3" in
  peq s (CFloatConstant (simplespan s, "1000.0001e-3"))

(*let test_parse_string_constant _ =
  let s = "\"test\"" in
  peq s (CStringConstant (simplespan s, "test"))
  let s = "\"test test\"" in
  peq s (CStringConstant (simplespan s, "test test"));
  let s = "\"test \\\"test\\\" test\"" in
  peq s (CStringConstant (simplespan s, "test \"test\" test"))*)

let test_parse_variable _ =
  let s = "a" in
  peq s (CVariable (simplespan s, i s));
  let s = "A" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test_Test" in
  peq s (CVariable (simplespan s, i s));
  let s = "Test_Test3" in
  peq s (CVariable (simplespan s, i s))

let test_parse_funcall_positional _ =
  let s = "f(true)" in
  peq s (CFuncall (simplespan s, i "f", ConcretePositionalArgs [CBoolConstant (span 1 2 1 6, true)]));
  let s = "f(true, nil)" in
  peq s (CFuncall (simplespan s, i "f", ConcretePositionalArgs [CBoolConstant (span 1 2 1 6, true); (CNilConstant (span 1 8 1 11))]))

(*let test_parse_funcall_named _ =
  let s = "f(a => true)" in
  peq s (CFuncall (simplespan s, i "f", ConcreteNamedArgs [(i "a", CBoolConstant true)]));
  let s = "f(a => true, b => nil)" in
  peq s (CFuncall (simplespan s, i "f", ConcreteNamedArgs [(i "a", CBoolConstant true); (i "b", CNilConstant)]))

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
 *)
let suite =
  "Expression parser" >::: [
      "Nil constant" >:: test_parse_nil_constant;
      "Bool constant" >:: test_parse_bool_constant;
      "Int constant" >:: test_parse_int_constant;
      "Float constant" >:: test_parse_float_constant;
      (*      "String constant" >:: test_parse_string_constant;*)
      "Variable" >:: test_parse_variable;
      "Function call, positional arguments" >:: test_parse_funcall_positional
      (*"Function call, named arguments" >:: test_parse_funcall_named;
      "Arithmetic" >:: test_parse_arithmetic*)
    ]

let _ = run_test_tt_main suite
