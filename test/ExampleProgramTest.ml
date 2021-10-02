open OUnit2
open Austral_core.Compiler
open TestUtil

(* A test of the simplest possible program: an entrypoint function that returns
   the root capability and does nothing else. *)
let test_empty_program _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    function Main(root: Root_Capability): Root_Capability is
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = compile_and_run [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

let suite =
  "Example programs" >::: [
      "Empty program" >:: test_empty_program
    ]

let _ = run_test_tt_main suite
