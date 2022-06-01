open Identifier
open IdentifierMap
open Error

type region = Region of int
[@@deriving eq, show]

let region_id (Region i) = i

let static_region_name = make_ident "Static"

let static_region = Region (0)

let region_counter: int ref = ref 1

let fresh_region _ =
  let i = !region_counter in
  region_counter := i + 1;
  Region i

type region_map = region IdentifierMap.t

let empty_region_map = IdentifierMap.empty

let get_region m n =
  IdentifierMap.find_opt n m

let add_region m n r =
  match get_region m n with
  | Some _ ->
     err "Region map already has a region with this name"
  | None ->
     IdentifierMap.add n r m
