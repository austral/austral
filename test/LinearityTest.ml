open OUnit2
open Austral_core.Compiler
open Austral_core.Error

(* Consume a linear record twice through a function call. This fails. *)
let test_funcall_twice _ =
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

    function Consume(r: R): Unit is
        let r: R := R(x => 32);
        return nil;
    end;

    function Main(root: Root_Capability): Root_Capability is
        let r: R := R(x => 32);
        Consume(r);
        Consume(r);
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

(* Test that you can't unbox a box twice. *)
let test_unbox_twice _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

import Austral.Memory (
    Pointer,
    Allocate,
    Load,
    Store,
    Deallocate
);

module body Example is
    pragma Unsafe_Module;

    record Box[T: Type]: Linear is
        pointer: Pointer[T];
    end;

    generic [T: Type]
    function Make(val: T): Option[Box[T]] is
        let ptr: Option[Pointer[T]] := Allocate(val);
        case ptr of
            when Some(value: Pointer[T]) do
                let box: Box[T] := Box(pointer => value);
                let boxopt: Option[Box[T]] := Some(value => box);
                return boxopt;
            when None do
                let boxopt: Option[Box[T]] := None();
                return boxopt;
        end case;
    end;

    generic [T: Type]
    function Unbox(box: Box[T]): T is
        let { pointer: Pointer[T] } := box;
        let value: T := Load(pointer);
        Deallocate(pointer);
        return value;
    end;

    function Main(root: Root_Capability): Root_Capability is
        let b: Option[Box[Integer_32]] := Make('e');
        case b of
            when Some(value: Box[Integer_32]) do
                Unbox(value);
                Unbox(value);
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

(* Test that a linear value created outside a loop cannot be used inside that
   loop. *)
let test_for_loop_with_external_linear_value _ =
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
        for i from 0 to 10 do
            let { x: Integer_32 } := r;
        end for;
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
      "Consume by calling a function twice" >:: test_funcall_twice;
      "Forget a case binding" >:: test_forget_case_binding;
      "Consume a case binding twice" >:: test_consume_case_binding_twice;
      "Unbox a box twice" >:: test_unbox_twice;
      "Create a linear value outside a loop and consume it inside" >:: test_for_loop_with_external_linear_value
    ]

let _ = run_test_tt_main suite
