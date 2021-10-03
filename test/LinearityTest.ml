open OUnit2
open Austral_core.Compiler
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

let suite =
  "Linearity checker tests" >::: [
      "Destructure record" >:: test_destructure_record
    ]

let _ = run_test_tt_main suite
