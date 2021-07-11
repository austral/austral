open Identifier

type region
[@@deriving eq, show]

val region_name : region -> identifier

val static_region_name : identifier

val static_region : region

val fresh_region : identifier -> region

type region_map

val empty_region_map : region_map

val add_region : region_map -> identifier -> region -> region_map

val get_region : region_map -> identifier -> region option
