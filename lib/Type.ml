open Identifier
open Region

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64

type signedness =
  | Unsigned
  | Signed

type type_parameter = TypeParameter of identifier * universe

type type_var = TypeVariable of identifier * universe

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | Array of ty * region
  | TyVar of type_var

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty

type type_signature = TypeSignature of identifier * type_parameter list * universe

let rec type_string = function
  | Unit -> "Unit"
  | Boolean -> "Boolean"
  | Integer (s, w) -> (signedness_string s) ^ "_" ^ (width_string w)
  | SingleFloat -> "SingleFloat"
  | DoubleFloat -> "DoubleFloat"
  | NamedType (n, args, _) -> (ident_string (local_name n)) ^ args_string args
  | Array (t, r) ->
     "Array[" ^ (type_string t) ^ ", " ^ (ident_string (region_name r)) ^ "]"
  | TyVar (TypeVariable (n, _)) -> ident_string n

and signedness_string = function
  | Unsigned -> "Natural"
  | Signed -> "Integer"

and width_int = function
  | Width8 -> 8
  | Width16 -> 16
  | Width32 -> 32
  | Width64 -> 64

and width_string w = string_of_int (width_int w)

and args_string = function
  | (first::rest) -> "[" ^ (String.concat ", " (List.map type_string (first::rest))) ^ "]"
  | [] -> ""

let size_type = Integer (Unsigned, Width64)

let string_type = Array (Integer (Unsigned, Width8), static_region)
