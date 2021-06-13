open Identifier
open Imports
open Cst
open Ast

let qualify_identifier m i =
  match get_symbol m i with
  | (Some q) -> q
  | None -> make_qident (importing_module m, i, i)

let rec qualify_typespec m (TypeSpecifier (n, args)) =
  QTypeSpecifier (qualify_identifier m n,
                  List.map (qualify_typespec m) args)
