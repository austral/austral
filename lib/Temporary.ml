(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

type tmp = Temporary of int

let counter: int ref = ref 0

let fresh_tmp _: tmp =
  let id: int = !counter in
  let tmp: tmp = Temporary id in
  counter := !counter + 1;
  tmp

let tmp_to_string (tmp: tmp): string =
  let (Temporary i) = tmp in
  "_t" ^ (string_of_int i)
