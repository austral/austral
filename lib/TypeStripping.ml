(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

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
  | SStaticArray of stripped_ty
  | SRegionTy of region
  | SReadRef of stripped_ty * stripped_ty
  | SWriteRef of stripped_ty * stripped_ty
  | SSpan of stripped_ty * stripped_ty
  | SSpanMut of stripped_ty * stripped_ty
  | SAddress of stripped_ty
  | SPointer of stripped_ty
  | SFnPtr of stripped_ty list * stripped_ty
  | SMonoTy of mono_id
  | SRegionTyVar of identifier * qident

let rec strip_type (ty: ty): stripped_ty =
  match strip_type' ty with
  | Some ty ->
     ty
  | None ->
     internal_err "strip_type called with a region type as its argument"

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
  | StaticArray elem_ty ->
     (match (strip_type' elem_ty) with
      | Some elem_ty ->
         Some (SStaticArray elem_ty)
      | None ->
         internal_err "array instantiated with a region type.")
  | RegionTy r ->
     Some (SRegionTy r)
  | ReadRef (ty, r) ->
     (match (strip_type' ty) with
      | Some ty ->
         (match (strip_type' r) with
          | Some r ->
             Some (SReadRef (ty, r))
          | None ->
             internal_err "unable to strip read ref type")
      | None ->
         internal_err "read ref instantiated with a region type.")
  | WriteRef (ty, r) ->
     (match (strip_type' ty) with
      | Some ty ->
         (match (strip_type' r) with
          | Some r ->
             Some (SWriteRef (ty, r))
          | None ->
             internal_err "unable to strip write ref type")
      | None ->
         internal_err "write ref instantiated with a region type.")
  | Span (ty, r) ->
     (match (strip_type' ty) with
      | Some ty ->
         (match (strip_type' r) with
          | Some r ->
             Some (SSpan (ty, r))
          | None ->
             internal_err "unable to strip span type")
      | None ->
         internal_err "span instantiated with a region type.")
  | SpanMut (ty, r) ->
     (match (strip_type' ty) with
      | Some ty ->
         (match (strip_type' r) with
          | Some r ->
             Some (SSpanMut (ty, r))
          | None ->
             internal_err "unable to strip span! type")
      | None ->
         internal_err "span! instantiated with a region type.")
  | TyVar (TypeVariable (name, u, source, _)) ->
     if u = RegionUniverse then
       Some (SRegionTyVar (name, source))
     else
       (* Why? Because when instantiating a monomorph, we do search and replace
          of type variables with their substitutions. So if there are variables
          left over by stripping time, that's an error. Anyways, the
          search-and-replace step should *also* have signalled an error if a
          type variable has no replacement. *)
       err ("strip_type': Variable not replaced: '" ^ (ident_string name) ^ "'")
  | Address ty ->
     (match (strip_type' ty) with
      | Some ty ->
         Some (SAddress ty)
      | None ->
         internal_err "Address type instantiated with a region type.")
  | Pointer ty ->
     (match (strip_type' ty) with
      | Some ty ->
         Some (SPointer ty)
      | None ->
         internal_err "Pointer type instantiated with a region type.")
  | FnPtr (args, rt) ->
     let rt =
       (match strip_type' rt with
        | Some ty -> ty
        | _ -> internal_err "Function pointer type instantiated with a region type.")
     in
     Some (SFnPtr (List.filter_map strip_type' args, rt))
  | MonoTy id ->
     Some (SMonoTy id)
