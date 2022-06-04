open OUnit2
open Austral_core.Compiler
open Austral_core.Error

(* Test that forgetting to consume a linear value bound in a `case` statement
   fails. *)
let test_forget_case_binding _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    record R : Linear is
        x: Integer_32;
    end;

    function Main(root: Root_Capability): Root_Capability is
        let r: R := R(x => 32);
        let ropt: Option[R] := Some(value => r);
        case ropt of
            when Some(value: R) do
                skip;
            when None do
                skip;
        end case;
        return root;
    end;
end module body.

|code}
  in
  try
    let _ = compile_and_run [(i, b)] "Example:Main" in
    assert_failure "This should have failed."
  with
    Austral_error _ ->
    assert_bool "Passed" true

(* Test that you can't consume a linear value twice in a case statement. *)
let test_consume_case_binding_twice _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    record R : Linear is
        x: Integer_32;
    end;

    function Consume(rec: R): Unit is
        let { x: Integer_32 } := rec;
        return nil;
    end;

    function Main(root: Root_Capability): Root_Capability is
        let r: R := R(x => 32);
        let ropt: Option[R] := Some(value => r);
        case ropt of
            when Some(value: R) do
                Consume(value);
                Consume(value);
            when None do
                skip;
        end case;
        return root;
    end;
end module body.

|code}
  in
  try
    let _ = compile_and_run [(i, b)] "Example:Main" in
    assert_failure "This should have failed."
  with
    Austral_error _ ->
    assert_bool "Passed" true

let suite =
  "Linearity checker tests" >::: [
      "Forget a case binding" >:: test_forget_case_binding;
      "Consume a case binding twice" >:: test_consume_case_binding_twice;
    ]

let _ = run_test_tt_main suite
