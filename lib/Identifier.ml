type identifier = string

let make_ident i = i
let ident_string i = i

type module_name = string

let make_mod_name n = n
let mod_name_string n = n

type qident = {
    source: module_name;
    original: identifier;
    local: identifier
  }

let make_qident (m, o, n) =
  { source = m; original = o; local = n }

let source_module_name { source; _ } = source
let original_name { original; _ } = original
let local_name { local; _ } = local
