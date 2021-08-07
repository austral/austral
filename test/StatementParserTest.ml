open OUnit2
open Austral_core.Cst
open Austral_core.ParserInterface
open TestUtil

let p = parse_stmt

let peq (s: string) (v: 'a) =
  eq v (p s)

let test_if _ =
  (* Discarding block *)
  let db e = CBlock [CDiscarding e]
  in
  peq "if true then nil; end if;" (CIf (CBoolConstant true, db CNilConstant, CSkip));
  peq "if true then nil; else nil; end if;" (CIf (CBoolConstant true, db CNilConstant, db CNilConstant));
  peq "if true then nil; else if false then nil; end if;" (CIf (CBoolConstant true, db CNilConstant, CIf (CBoolConstant false, db CNilConstant, CSkip)));
  peq "if true then nil; else if false then nil; else nil; end if;" (CIf (CBoolConstant true, db CNilConstant, CIf (CBoolConstant false, db CNilConstant, db CNilConstant)))

let test_return _ =
  peq "return 123;" (CReturn (CIntConstant "123"))


let test_skip _ =
  peq "skip;" CSkip

let suite =
  "Statement parser" >::: [
      "If statement" >:: test_if;
      "Return statement" >:: test_return;
      "Skip statement" >:: test_skip
    ]

let _ = run_test_tt_main suite
