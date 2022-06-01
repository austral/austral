open Region

module RegionSet =
  Set.Make(
      struct
        type t = region
        let compare a b = compare (region_id a) (region_id b)
      end
    )
