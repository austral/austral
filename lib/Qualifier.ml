open Identifier
open Imports

let qualify_identifier m i =
  match get_symbol m i with
  | (Some q) -> q
  | None -> make_qident (importing_module m, i, i)

          (*
let qualify_typespec m t =
  err "Not implemented"
           *)
