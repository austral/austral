module IdentifierSet =
  Set.Make(
      struct
        open Identifier
        type t = identifier
        let compare a b = compare (ident_string a) (ident_string b)
      end
    )

module SIdentSet =
  Set.Make(
      struct
        open Identifier
        type t = sident
        let compare a b = compare (show_sident a) (show_sident b)
      end
    )
