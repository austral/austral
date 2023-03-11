(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open OUnit2
open Austral_core.CRenderer
open Austral_core.Escape

let r_e = render_expr
let test_render_bool _ =
  assert_equal (r_e (CBool true)) "true";
  assert_equal (r_e (CBool false)) "false"

let test_render_string _ =
  assert_equal (r_e (CString  (escape_string "abcd"))) "\"abcd\"";
  assert_equal (r_e (CString (escape_string "\""))) "\"\\\"\"";
  assert_equal (r_e (CString (escape_string "\"\"\""))) "\"\\\"\\\"\\\"\""
  
  
  let suite =
    "CliUtil" >::: [
        "render_bool" >:: test_render_bool;
        "render_string" >:: test_render_string
      ]
  
  let _ = run_test_tt_main suite
  