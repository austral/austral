(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open Region
open Names
open Sexplib
open Std

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse
[@@deriving (eq, show, sexp)]

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64
  | WidthByteSize
  | WidthIndex
[@@deriving (eq, show, sexp)]

type signedness =
  | Unsigned
  | Signed
[@@deriving (eq, show, sexp)]

type type_var = TypeVariable of identifier * universe * qident * sident list
[@@deriving (eq, show, sexp)]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | StaticArray of ty
  | RegionTy of region
  | ReadRef of ty * ty
  | WriteRef of ty * ty
  | TyVar of type_var
  | Address of ty
  | Pointer of ty
  | FnPtr of ty list * ty
  | MonoTy of mono_id
[@@deriving (show, sexp)]

type typed_slot = TypedSlot of identifier * ty
[@@deriving sexp]

type typed_case = TypedCase of identifier * typed_slot list
[@@deriving sexp]

type value_parameter = ValueParameter of identifier * ty
[@@deriving (show, sexp)]

let universe_string = function
  | FreeUniverse -> "Free"
  | LinearUniverse -> "Linear"
  | TypeUniverse -> "Type"
  | RegionUniverse -> "Region"

let rec type_string = function
  | Unit ->
     unit_name
  | Boolean ->
     bool_name
  | Integer (s, w) ->
     let sgn: string =
       match s with
       | Unsigned -> nat_prefix
       | Signed -> int_prefix
     in
     (match w with
      | Width8 -> sgn ^ "8"
      | Width16 -> sgn ^ "16"
      | Width32 -> sgn ^ "32"
      | Width64 -> sgn ^ "64"
      | WidthByteSize -> "ByteSize"
      | WidthIndex -> "Index")
  | SingleFloat ->
     single_float_name
  | DoubleFloat ->
     double_float_name
  | NamedType (n, args, _) ->
     (ident_string (local_name n)) ^ args_string args
  | StaticArray t ->
     "FixedArray[" ^ (type_string t) ^ "]"
  | RegionTy r ->
     "Region<" ^ (string_of_int (region_id r)) ^ ">"
  | ReadRef (t, r) ->
     read_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string r) ^ "]"
  | WriteRef (t, r) ->
     write_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string r) ^ "]"
  | TyVar (TypeVariable (n, _, _, _)) ->
     ident_string n
  | Address ty ->
     address_name ^ "[" ^ (type_string ty) ^ "]"
  | Pointer ty ->
     pointer_name ^ "[" ^ (type_string ty) ^ "]"
  | FnPtr (args, rt) ->
     (match args with
      | [] ->
         (* No arguments case. *)
         ("Fn[" ^ (type_string rt) ^ "]")
      | _ ->
         (* Some arguments case. *)
         let args': string = String.concat ", " (List.map type_string args) in
         ("Fn[" ^ args' ^ ", " ^ (type_string rt) ^ "]"))
  | MonoTy _ ->
     "MonoTy)"

and args_string = function
  | (first::rest) -> "[" ^ (String.concat ", " (List.map type_string (first::rest))) ^ "]"
  | [] -> ""

let index_type = Integer (Unsigned, WidthIndex)

let string_type = StaticArray (Integer (Unsigned, Width8))

let rec equal_ty a b =
  match a with
  | Unit ->
     (match b with
      | Unit ->
         true
      | _ ->
         false)
  | Boolean ->
     (match b with
      | Boolean ->
         true
      | _ ->
         false)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         (equal_signedness s s') && (equal_integer_width w w')
      | _ ->
         false)
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         true
      | _ ->
         false)
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         true
      | _ ->
         false)
  | NamedType (n, args, u) ->
     (match b with
      | NamedType (n', args', u') ->
         (equal_qident n n')
         && (List.for_all (fun (a', b') -> equal_ty a' b') (List.map2 (fun a' b' -> (a',b')) args args'))
         && (equal_universe u u')
      | _ ->
         false)
  | StaticArray t ->
     (match b with
      | StaticArray t' ->
         equal_ty t t'
      | _ ->
         false)
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         equal_region r r'
      | _ ->
         false)
  | ReadRef (ty, r) ->
     (match b with
      | ReadRef (ty', r') ->
         (equal_ty ty ty') && (equal_ty r r')
      | _ ->
         false)
  | WriteRef (ty, r) ->
     (match b with
      | WriteRef (ty', r') ->
         (equal_ty ty ty') && (equal_ty r r')
      | _ ->
         false)
  | TyVar v ->
     (match b with
      | TyVar v' ->
         equal_type_var v v'
      | _ ->
         false)
  | Address ty ->
     (match b with
      | Address ty' ->
         equal_ty ty ty'
      | _ ->
        false)
  | Pointer ty ->
     (match b with
      | Pointer ty' ->
         equal_ty ty ty'
      | _ ->
         false)
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') ->
         (List.for_all (fun (a', b') -> equal_ty a' b') (List.map2 (fun a' b' -> (a', b')) args args'))
         && (equal_ty rt rt')
      | _ ->
        false)
  | MonoTy a ->
    (match b with
     | MonoTy b ->
       equal_mono_id a b
     | _ ->
       false)
