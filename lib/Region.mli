open Identifier

type region
[@@deriving eq]

val region_name : region -> identifier

val static_region_name : identifier

val static_region : region

val fresh_region : identifier -> region
