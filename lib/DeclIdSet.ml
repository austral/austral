module DeclIdSet =
  Set.Make(
      struct
        open Id
        type t = decl_id
        let compare (a: decl_id) (b: decl_id): int =
          let DeclId a' = a
          and DeclId b' = b in
          compare a' b'
      end
    )
