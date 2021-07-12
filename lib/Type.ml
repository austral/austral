open Identifier
open Region

type universe =
  | FreeUniverse
  | LinearUniverse
  | TypeUniverse
  | RegionUniverse
[@@deriving eq, show]

type integer_width =
  | Width8
  | Width16
  | Width32
  | Width64
[@@deriving eq, show]

type signedness =
  | Unsigned
  | Signed
[@@deriving eq, show]

type type_parameter = TypeParameter of identifier * universe

type type_var = TypeVariable of identifier * universe
[@@deriving eq, show]

type ty =
  | Unit
  | Boolean
  | Integer of signedness * integer_width
  | SingleFloat
  | DoubleFloat
  | NamedType of qident * ty list * universe
  | Array of ty * region
  | RegionTy of region
  | ReadRef of ty * region
  | TyVar of type_var
[@@deriving show]

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty

type type_signature = TypeSignature of identifier * type_parameter list * universe

let universe_string = function
  | FreeUniverse -> "Free"
  | LinearUniverse -> "Linear"
  | TypeUniverse -> "Type"
  | RegionUniverse -> "Region"

let rec type_string = function
  | Unit -> "Unit"
  | Boolean -> "Boolean"
  | Integer (s, w) -> (signedness_string s) ^ "_" ^ (width_string w)
  | SingleFloat -> "SingleFloat"
  | DoubleFloat -> "DoubleFloat"
  | NamedType (n, args, _) -> (ident_string (local_name n)) ^ args_string args
  | Array (t, r) ->
     "Array[" ^ (type_string t) ^ ", " ^ (ident_string (region_name r)) ^ "]"
  | RegionTy r ->
     ident_string (region_name r)
  | ReadRef (t, r) ->
     "Reference[" ^ (type_string t) ^ ", " ^ (ident_string (region_name r)) ^ "]"
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

let rec equal_ty a b =
  match (a, b) with
  | (Unit, Unit) ->
     true
  | (Boolean, Boolean) ->
     true
  | (Integer (s, w), Integer (s', w')) ->
     (equal_signedness s s') && (equal_integer_width w w')
  | (SingleFloat, SingleFloat) ->
     true
  | (DoubleFloat, DoubleFloat) ->
     true
  | (NamedType (n, args, u), NamedType (n', args', u')) ->
     (equal_qident n n')
     && (List.for_all (fun (a', b') -> equal_ty a' b') (List.map2 (fun a' b' -> (a',b')) args args'))
     && (equal_universe u u')
  | (Array (t, r), Array (t', r')) ->
     (equal_ty t t') && (equal_region r r')
  | (TyVar v, TyVar v') ->
     equal_type_var v v'
  | _ ->
     false
