open Identifier
open Type
open Sexplib
open Std

type type_parameter = TypeParameter of identifier * universe * typaram_source * sident list
[@@deriving (show, sexp)]

let make_typaram (i, u, s, cs) =
  TypeParameter (i, u, s, cs)

let typaram_name (TypeParameter (i, _, _, _)) =
  i

let typaram_universe (TypeParameter (_, u, _, _)) =
  u

let typaram_source (TypeParameter (_, _, s, _)) =
  s

let typaram_constraints (TypeParameter (_, _, _, cs)) =
  cs

let typaram_to_tyvar (typaram: type_parameter): type_var =
  let (TypeParameter (n, u, f, cs)) = typaram in
  TypeVariable (n, u, f, cs)

type unsourced_typaram = UnsourcedTyparam of identifier * universe * sident list

let make_unsourced_typaram (i, u, cs) =
  UnsourcedTyparam (i, u, cs)

let link_typaram (utp: unsourced_typaram) (source: typaram_source): type_parameter =
  let (UnsourcedTyparam (name, universe, constraints)) = utp in
  TypeParameter (name, universe, source, constraints)
