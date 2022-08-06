(** Stores the compiler's version. *)

type version = int * int * int

let version: version = (0, 0, 6)

let version_string: string =
  let (mj, mi, p) = version
  and s = string_of_int in
  let (mj, mi, p) = (s mj, s mi, s p) in
  mj ^ "." ^ mi ^ "." ^ p
