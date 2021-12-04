open Identifier
open Type
open Error

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

let mono_type_counter: int ref = ref 1

let fresh_mono_type_id _ =
  let id = !mono_type_counter in
  mono_type_counter := id + 1;
  id

let add_monomorph (tbl: mono_type_tbl) (name: qident) (args: mono_ty list): (mono_type_id * mono_type_tbl) =
  match get_monomorph tbl name args with
  | Some _ ->
     err "Monomorph exists in table."
  | None ->
     let id = fresh_mono_type_id () in
     (id, (name, args, id) :: tbl)

type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SArray of stripped_ty
  | SReadRef of stripped_ty
  | SWriteRef of stripped_ty
  | SRawPointer of stripped_ty

let rec strip_type (ty: ty): stripped_ty option =
  match ty with
  | Unit ->
     Some SUnit
  | Boolean ->
     Some SBoolean
  | Integer (s, w) ->
     Some (SInteger (s, w))
  | SingleFloat ->
     Some SSingleFloat
  | DoubleFloat ->
     Some SDoubleFloat
  | NamedType (n, args, _) ->
     Some (SNamedType (n, List.filter_map strip_type args))
  | Array (elem_ty, _) ->
     (match (strip_type elem_ty) with
      | Some elem_ty ->
         Some (SArray elem_ty)
      | None ->
         err "Internal: array instantiated with a region type.")
  | RegionTy _ ->
     None
  | ReadRef (ty, _) ->
     (match (strip_type ty) with
      | Some ty ->
         Some (SReadRef ty)
      | None ->
         err "Internal: read ref instantiated with a region type.")
  | WriteRef (ty, _) ->
     (match (strip_type ty) with
      | Some ty ->
         Some (SWriteRef ty)
      | None ->
         err "Internal: write ref instantiated with a region type.")
  | TyVar _ ->
     None
  | RawPointer ty ->
     (match (strip_type ty) with
      | Some ty ->
         Some (SRawPointer ty)
      | None ->
         err "Internal: raw pointer type instantiated with a region type.")

let monomorphize_type _ _: (mono_ty * mono_type_tbl) =
  err "Not implemented"
