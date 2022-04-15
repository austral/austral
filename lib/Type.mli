open Id
open Identifier
open Region

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
[@@deriving eq]

type signedness =
  | Unsigned
  | Signed
[@@deriving eq]

type type_parameter = TypeParameter of identifier * universe * qident

type type_var = TypeVariable of identifier * universe * qident
[@@deriving eq]

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
  | RawPointer of ty
  | MonoTy of mono_id
  (** Special case, see the `mono_to_ty` function. We need this to be able to do
     monomorph instantiation, but this doesn't correspond to anything in the
     code. *)
[@@deriving show]

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

val universe_string : universe -> string

(* A string representation of a type, for debugging *)
val type_string : ty -> string

(* The type of array sizes and indices *)
val size_type : ty

val string_type : ty

val width_int : integer_width -> int

val equal_ty : ty -> ty -> bool
