module TypeVarSet =
  Set.Make(
      struct
        open Type
        type t = type_var
        let compare = compare
      end
    )
