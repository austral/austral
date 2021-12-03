open OUnit2
open Austral_core.Compiler
open Austral_core.Error
open TestUtil

let print_and_reraise_error (f: unit -> 'a): 'a =
  try
    f ()
  with
    Austral_error error ->
     Printf.eprintf "%s" (render_error error None);
     raise (Austral_error error)

let car sources entrypoint =
  print_and_reraise_error (fun _ -> compile_and_run sources entrypoint)

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
  let (code, stdout) = car [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

(* A test of the @embed intrinsic. *)
let test_embed_intrinsic _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    pragma Unsafe_Module;

    function Put_Character(character: Natural_8): Integer_32 is
        pragma Foreign_Import(External_Name => "putchar");
    end;

    function Main(root: Root_Capability): Root_Capability is
        let char: Natural_8 := @embed(Natural_8, "$1 + $2", 90, 7);
        Put_Character(char);
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = car [(i, b)] "Example:Main" in
  eq 0 code;
  eq "a" stdout

(* Test that integer literals work in constructor calls. *)
let test_integer_literal_record_constructor _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    record R: Free is
        x: Natural_8;
        y: Integer_8;
    end;

    function Main(root: Root_Capability): Root_Capability is
        let r: R := R(x => 10, y => 10);
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = car [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

(* Test that the trapping arithmetic typeclass produces expected results. *)
let test_trapping_arithmetic _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|
module body Example is
    function Main(root: Root_Capability): Root_Capability is
        let res: Integer_32 := Trapping_Add(10, 10);
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = car [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

(* A test of the sizeof operator. *)
let test_sizeof_operator _ =
  let i = {code|

module Example is
    function Main(root: Root_Capability): Root_Capability;
end module.

|code}
  and b = {code|

module body Example is
    pragma Unsafe_Module;

    function Put_Character(character: Natural_8): Integer_32 is
        pragma Foreign_Import(External_Name => "putchar");
    end;

    function Main(root: Root_Capability): Root_Capability is
        let size: Natural_64 := sizeof(Natural_8);
        if size /= 1 then
            Put_Character('f');
        end if;
        return root;
    end;
end module body.

|code}
  in
  let (code, stdout) = car [(i, b)] "Example:Main" in
  eq 0 code;
  eq "" stdout

let suite =
  "Example programs" >::: [
      "Empty program" >:: test_empty_program;
      "@embed intrinsic" >:: test_embed_intrinsic;
      "Integer literals in record constructors" >:: test_integer_literal_record_constructor;
      "Trapping arithmetic" >:: test_trapping_arithmetic;
      "sizeof operator" >:: test_sizeof_operator
    ]

let _ = run_test_tt_main suite
