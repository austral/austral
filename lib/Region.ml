type region = Region of int

let static_region_name = "Static"

let static_region = Region 0

let region_counter: int ref = ref 1

let fresh_region _ =
  let i = !region_counter in
  region_counter := i + 1;
  Region i
