open Identifier
open Type
open Sexplib
open Std

type type_parameter = TypeParameter of identifier * universe * qident * sident list
[@@deriving (show, sexp)]

let make_typaram (i, u, q, s) =
  TypeParameter (i, u, q, s)

let typaram_name (TypeParameter (i, _, _, _)) =
  i

let typaram_universe (TypeParameter (_, u, _, _)) =
  u

let typaram_constraints (TypeParameter (_, _, _, cs)) =
  cs
