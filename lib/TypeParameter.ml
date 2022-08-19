open Identifier
open Id
open Type
open Sexplib
open Std

type typaram_source =
  | DeclSource of decl_id
  | MethodSource of ins_meth_id

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
