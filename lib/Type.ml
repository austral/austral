open Id
open Identifier
open Region
open Names

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse
[@@deriving (eq, show)]

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64
  | WidthIndex
[@@deriving (eq, show)]

type signedness =
  | Unsigned
  | Signed
[@@deriving (eq, show)]

type type_parameter = TypeParameter of identifier * universe * qident
[@@deriving show]

type type_var = TypeVariable of identifier * universe * qident
[@@deriving (eq, show)]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | Array of ty * region
  | RegionTy of region
  | ReadRef of ty * ty
  | WriteRef of ty * ty
  | TyVar of type_var
  | Address of ty
  | Pointer of ty
  | MonoTy of mono_id
[@@deriving show]

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty
[@@deriving show]

type type_signature = TypeSignature of identifier * type_parameter list * universe

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
       | Unsigned -> "Natural_"
       | Signed -> "Integer_"
     in
     (match w with
      | Width8 -> sgn ^ "8"
      | Width16 -> sgn ^ "16"
      | Width32 -> sgn ^ "32"
      | Width64 -> sgn ^ "64"
      | WidthIndex -> "Index")
  | SingleFloat ->
     single_float_name
  | DoubleFloat ->
     double_float_name
  | NamedType (n, args, u) ->
     (qident_debug_name n) ^ args_string args ^ ": " ^ (universe_string u)
  | Array (t, r) ->
     "Array[" ^ (type_string t) ^ ", " ^ (ident_string (region_name r)) ^ "]: Free"
  | RegionTy r ->
     ident_string (region_name r) ^ "(" ^ (string_of_int (region_id r)) ^ ")"
  | ReadRef (t, r) ->
     read_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string r) ^ "]: Free"
  | WriteRef (t, r) ->
     write_ref_name ^ "[" ^ (type_string t) ^ ", " ^ (type_string r) ^ "]: Linear"
  | TyVar (TypeVariable (n, u, from)) ->
     (ident_string n) ^ "(" ^ (qident_debug_name from) ^ ")" ^ ": " ^ (universe_string u)
  | Address ty ->
     address_name ^ "[" ^ (type_string ty) ^ "]"
  | Pointer ty ->
     pointer_name ^ "[" ^ (type_string ty) ^ "]"
  | MonoTy _ ->
    "MonoTy"

and args_string = function
  | (first::rest) -> "[" ^ (String.concat ", " (List.map type_string (first::rest))) ^ "]"
  | [] -> ""

let size_type = Integer (Unsigned, WidthIndex)

let string_type = Array (Integer (Unsigned, Width8), static_region)

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
  | Array (t, r) ->
     (match b with
      | Array (t', r') ->
         (equal_ty t t') && (equal_region r r')
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
  | MonoTy a ->
    (match b with
     | MonoTy b ->
       equal_mono_id a b
     | _ ->
       false)
