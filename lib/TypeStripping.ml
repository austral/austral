open Type
open Id
open Identifier
open Region
open Error

type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SArray of stripped_ty * region
  | SRegionTy of region
  | SReadRef of stripped_ty * stripped_ty
  | SWriteRef of stripped_ty * stripped_ty
  | SRawPointer of stripped_ty
  | SMonoTy of mono_id

let rec strip_type (ty: ty): stripped_ty =
  match strip_type' ty with
  | Some ty ->
    ty
  | None ->
    err "strip_type called with a region type as its argument"

and strip_type' (ty: ty): stripped_ty option =
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
    Some (SNamedType (n, List.filter_map strip_type' args))
  | Array (elem_ty, r) ->
    (match (strip_type' elem_ty) with
     | Some elem_ty ->
       Some (SArray (elem_ty, r))
     | None ->
       err "Internal: array instantiated with a region type.")
  | RegionTy r ->
    Some (SRegionTy r)
  | ReadRef (ty, r) ->
    (match (strip_type' ty) with
     | Some ty ->
       (match (strip_type' r) with
        | Some r ->
          Some (SReadRef (ty, r))
        | None ->
          err "internal")
     | None ->
       err "Internal: read ref instantiated with a region type.")
  | WriteRef (ty, r) ->
    (match (strip_type' ty) with
     | Some ty ->
       (match (strip_type' r) with
        | Some r ->
          Some (SWriteRef (ty, r))
        | None ->
          err "internal")
     | None ->
       err "Internal: write ref instantiated with a region type.")
  | TyVar (TypeVariable (name, _, _)) ->
    (* Why? Because when instantiating a monomorph, we do search and replace of
       type variables with their substitutions. So if there are variables left
       over by stripping time, that's an error. Anyways, the search-and-replace
       step should *also* have signalled an error if a type variable has no
       replacement. *)
    err ("strip_type': Variable not replaced: '" ^ (ident_string name) ^ "'")
  | RawPointer ty ->
    (match (strip_type' ty) with
     | Some ty ->
       Some (SRawPointer ty)
     | None ->
       err "Internal: raw pointer type instantiated with a region type.")
  | MonoTy id ->
    Some (SMonoTy id)
