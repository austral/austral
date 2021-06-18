open Identifier

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
  | TyVar of type_var

type typed_slot = TypedSlot of identifier * ty

type typed_case = TypedCase of identifier * typed_slot list

type value_parameter = ValueParameter of identifier * ty

(* Represents the interface to a type: its name, list of type parameters, and
   universe. We use this as part of the semantic extraction process: to validate
   the definition of a type, we need to know about other types defined in the
   module. To break the circularity, we have a pass that extracts type
   signatures first, and then validates type structures against those
   signatures.  *)
type type_signature = TypeSignature of identifier * type_parameter list * universe

(* A string representation of a type, for debugging *)
val type_string : ty -> string
