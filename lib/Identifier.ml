type identifier = string
[@@deriving eq, show]

let make_ident i = i
let ident_string i = i

type module_name = string
[@@deriving eq, show]

let make_mod_name n = n
let mod_name_string n = n

type sident = module_name * identifier

let make_sident mn n =
  (mn, n)

let sident_module_name (mn, _) = mn
let sident_name (_, n) = n

type qident = {
    source: module_name;
    original: identifier;
    local: identifier
  }
[@@deriving show]

let make_qident (m, o, n) =
  { source = m; original = o; local = n }

let source_module_name { source; _ } = source
let original_name { original; _ } = original
let local_name { local; _ } = local

let qident_debug_name { source; original; local } =
  let s = mod_name_string source
  and o = ident_string original
  and l = ident_string local
  in
  l ^ " from " ^ s ^ (if o = l then "" else (", originally " ^ o))

let equal_qident { source; original; _ } { source=source'; original=original'; _ } =
  (equal_module_name source source') && (equal_identifier original original')

let qident_to_sident { source; original; _ }: sident =
  (source, original)
