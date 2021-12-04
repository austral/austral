open Identifier
open Type

type mono_type_id = int
[@@deriving eq]

type mono_ty =
  | MonoUnit
  | MonoBoolean
  | MonoInteger of signedness * integer_width
  | MonoSingleFloat
  | MonoDoubleFloat
  | MonoNamedType of qident * mono_type_id
  | MonoArray of mono_ty
  | MonoReadRef of mono_ty
  | MonoWriteRef of mono_ty
  | MonoRawPointer of mono_ty
[@@deriving eq]

type mono_type_tbl = (qident * mono_ty list * mono_type_id) list

let empty_mono_type_tbl: mono_type_tbl = []

let get_monomorph (tbl: mono_type_tbl) (name: qident) (args: mono_ty list): mono_type_id option =
  let filter (name', args', id) =
    if (equal_qident name name') && (List.for_all2 equal_mono_ty args args') then
      Some id
    else
      None
  in
  List.find_map filter tbl
