open Identifier
open Type
open Error

type mono_id = int
[@@deriving eq]

type mono_status =
  | NotInstantiated
  | Instantiated

type mono_ty =
  | MonoUnit
  | MonoBoolean
  | MonoInteger of signedness * integer_width
  | MonoSingleFloat
  | MonoDoubleFloat
  | MonoNamedType of qident * mono_id
  | MonoArray of mono_ty
  | MonoReadRef of mono_ty
  | MonoWriteRef of mono_ty
  | MonoRawPointer of mono_ty
[@@deriving eq]

type mono_tbl = (qident * mono_ty list * mono_id * mono_status) list

let empty_mono_tbl: mono_tbl = []

let get_monomorph_id (tbl: mono_tbl) (name: qident) (args: mono_ty list): mono_id option =
  let filter (name', args', id, _) =
    if (equal_qident name name') && (List.for_all2 equal_mono_ty args args') then
      Some id
    else
      None
  in
  List.find_map filter tbl

let mono_type_counter: int ref = ref 1

let fresh_mono_id _ =
  let id = !mono_type_counter in
  mono_type_counter := id + 1;
  id

let add_monomorph (tbl: mono_tbl) (name: qident) (args: mono_ty list): (mono_id * mono_tbl) =
  match get_monomorph_id tbl name args with
  | Some _ ->
     err "Monomorph exists in table."
  | None ->
     let id = fresh_mono_id () in
     (id, (name, args, id, NotInstantiated) :: tbl)
