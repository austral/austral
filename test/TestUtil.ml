open OUnit2
open Austral_core.Identifier
open Austral_core.Span

let eq = assert_equal
let i = make_ident

let span l c l' c' =
  Span {
      filename = "";
      startp = Position {
                   line = l;
                   column = c;
                 };
      endp = Position {
                 line = l';
                 column = c';
               };
  }

let simplespan s =
  span 1 0 1 (String.length s)
