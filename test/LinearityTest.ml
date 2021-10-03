open OUnit2
open Austral_core.Compiler
open Austral_core.Error
open TestUtil

(* A straightforward test: create a linear record and consume it by destructuring. *)
let test_destructure_record _ =
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
        let { x: Integer_32 } := r;
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = compile_and_run [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

(* Create a linear record. Do nothing. This throws an error. *)
let test_forget_record _ =
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
        let r: R := R(x => 32);3
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

(* Consume a linear value in both branches of an if statement. *)
let test_consume_if _ =
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
        if true then
            let { x: Integer_32 } := r;
        else
            let { x: Integer_32 } := r;
        end if;
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = compile_and_run [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

(* Consume a linear value in one branch of an if statement. This fails. *)
let test_consume_if_asymmetrically _ =
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
        if true then
            let { x: Integer_32 } := r;
        else
            skip;
        end if;
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
      "Destructure record" >:: test_destructure_record;
      "Forget record" >:: test_forget_record;
      "Consume record in if statement" >:: test_consume_if;
      "Consume record in one branch of an if statement" >:: test_consume_if_asymmetrically
    ]

let _ = run_test_tt_main suite
