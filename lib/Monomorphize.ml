open Identifier
open Type
open MonoType
open Tast
open Mtast
open Error

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

let rec strip_type (ty: ty): stripped_ty =
  match ty with
  | Unit ->
     SUnit
  | Boolean ->
     SBoolean
  | Integer (s, w) ->
     SInteger (s, w)
  | SingleFloat ->
     SSingleFloat
  | DoubleFloat ->
     SDoubleFloat
  | NamedType (n, args, _) ->
     SNamedType (n, List.map strip_type args)
  | Array (elem_ty, _) ->
     SArray (strip_type elem_ty)
  | RegionTy _ ->
     err "Region type."
  | ReadRef (ty, _) ->
     SReadRef (strip_type ty)
  | WriteRef (ty, _) ->
     SWriteRef (strip_type ty)
  | TyVar _ ->
     err "Type variable not yet replaced."
  | RawPointer ty ->
     SRawPointer (strip_type ty)

let rec monomorphize_type (tbl: mono_tbl) (ty: stripped_ty): (mono_ty * mono_tbl) =
  match ty with
  | SUnit ->
     (MonoUnit, tbl)
  | SBoolean ->
     (MonoBoolean, tbl)
  | SInteger (s, w) ->
     (MonoInteger (s, w), tbl)
  | SSingleFloat ->
     (MonoSingleFloat, tbl)
  | SDoubleFloat ->
     (MonoDoubleFloat, tbl)
  | SArray elem_ty ->
     let (elem_ty, tbl) = monomorphize_type tbl elem_ty in
     (MonoArray elem_ty, tbl)
  | SReadRef ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoReadRef ty, tbl)
  | SWriteRef ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoWriteRef ty, tbl)
  | SRawPointer ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoRawPointer ty, tbl)
  | SNamedType (name, args) ->
     let (args, tbl) = monomorphize_list tbl args in
     (match get_monomorph_id tbl name args with
      | Some id ->
         (MonoNamedType (name, id), tbl)
      | None ->
         let (id, tbl) = add_monomorph tbl name args in
         (MonoNamedType (name, id), tbl))

and monomorphize_list (tbl: mono_tbl) (tys: stripped_ty list): (mono_ty list * mono_tbl) =
  match tys with
  | first::rest ->
     let (first, tbl) = monomorphize_type tbl first in
     let (rest, tbl) = monomorphize_list tbl rest in
     (first :: rest, tbl)
  | [] ->
     ([], tbl)

let rec monomorphize_expr (tbl: mono_tbl) (expr: texpr): (mexpr * mono_tbl) =
  match expr with
  | TNilConstant ->
     (MNilConstant, tbl)
  | TBoolConstant b ->
     (MBoolConstant b, tbl)
  | TIntConstant i ->
     (MIntConstant i, tbl)
  | TFloatConstant f ->
     (MFloatConstant f, tbl)
  | _ ->
     err "Not done yet"
