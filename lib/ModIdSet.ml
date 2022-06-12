module ModIdSet =
  Set.Make(
      struct
        open Id
        type t = mod_id
        let compare (a: t) (b: t): int =
          let ModId a' = a
          and ModId b' = b in
          compare a' b'
      end
    )
