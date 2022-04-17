type file_id = FileId of int
[@@deriving eq]

type mod_id = ModId of int
[@@deriving eq]

type decl_id = DeclId of int
[@@deriving (eq, show)]

type ins_meth_id = InsMethId of int
[@@deriving (eq, show)]

type mono_id = MonoId of int
[@@deriving (eq, show)]

(* ID utilities *)

let file_counter: int ref = ref 1

let fresh_file_id _: file_id =
  let id = !file_counter in
  file_counter := id + 1;
  FileId id

let mod_counter: int ref = ref 1

let fresh_mod_id _: mod_id =
  let id = !mod_counter in
  mod_counter := id + 1;
  ModId id

let decl_counter: int ref = ref 1

let fresh_decl_id _: decl_id =
  let id = !decl_counter in
  decl_counter := id + 1;
  DeclId id

let ins_meth_counter: int ref = ref 1

let fresh_ins_meth_id _: ins_meth_id =
  let id = !ins_meth_counter in
  ins_meth_counter := id + 1;
  InsMethId id

let mono_counter: int ref = ref 1

let fresh_mono_id _: mono_id =
  let id = !mono_counter in
  mono_counter := id + 1;
  MonoId id
