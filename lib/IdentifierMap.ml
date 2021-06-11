module IdentifierMap =
  Map.Make(
      struct
        open Identifier
        type t = identifier
        let compare a b = compare (ident_string a) (ident_string b)
      end
    )
