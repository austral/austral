open Id
open Identifier
open Region

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
  | WidthIndex
[@@deriving (eq, show, sexp)]

type signedness =
  | Unsigned
  | Signed
[@@deriving (eq, show, sexp)]

(** The place where a type parameter was defined: either a declaration or an instance method. *)
type typaram_source =
  | DeclSource of decl_id
  | MethodSource of ins_meth_id
[@@deriving (eq, show, sexp)]

type type_var = TypeVariable of identifier * universe * typaram_source * sident list
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
  (** Special case, see the `mono_to_ty` function. We need this to be able to do
     monomorph instantiation, but this doesn't correspond to anything in the
     code. *)
[@@deriving (show, sexp)]

type typed_slot = TypedSlot of identifier * ty
[@@deriving sexp]

type typed_case = TypedCase of identifier * typed_slot list
[@@deriving sexp]

type value_parameter = ValueParameter of identifier * ty
[@@deriving (show, sexp)]

val universe_string : universe -> string

(* A string representation of a type, for debugging *)
val type_string : ty -> string

(* The type of array sizes and indices *)
val size_type : ty

val string_type : ty

val equal_ty : ty -> ty -> bool
