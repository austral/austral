open Identifier

type region = Region of identifier * int

let region_name (Region (n, _)) = n

let static_region_name = make_ident "Static"

let static_region = Region (static_region_name, 0)

let region_counter: int ref = ref 1

let fresh_region n =
  let i = !region_counter in
  region_counter := i + 1;
  Region (n, i)
