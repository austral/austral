open Identifier
open Type
open Tast
open Cpp
open Error

(* Codegen implementation notes:

   A union like this:

       union Optional_Int is
           case Present is
               value: Integer_32;
           case Absent;
       end union;

   Is represented like this:

       enum Optional_Int_Tag {
           PRESENT,
           ABSENT
       }

       typedef struct {
           Optional_Int_Tag tag;
           union {
               struct { int32_t value; } Present;
               struct {} Absent;
           } data;
       } Optional_Int;

*)


(* Identifiers *)

let gen_ident (i: identifier): string =
  (ident_string i)

let gen_module_name (n: module_name): string =
  Str.global_replace (Str.regexp "\\.") "__" (mod_name_string n)

let gen_qident (i: qident): string =
  (gen_module_name (source_module_name i)) ^ "::" ^ (gen_ident (original_name i))

(* Types *)

let is_special_type (_: qident): bool =
  false

let gen_special_type (_: qident) (_: ty list): cpp_ty =
  err "TODO"

let rec gen_type (ty: ty): cpp_ty =
  let t s = CNamedType (s, []) in
  match ty with
  | Unit ->
     t "bool"
  | Boolean ->
     t "bool"
  | Integer (s, w) ->
     let s' = (match s with
               | Unsigned -> "u"
               | Signed -> "")
     and w' = string_of_int (width_int w) in
     t (s' ^ "int" ^ w' ^ "_t")
  | SingleFloat ->
     t "float"
  | DoubleFloat ->
     t "double"
  | NamedType (n, a, _) ->
     if is_special_type n then
       gen_special_type n a
     else
       CNamedType (gen_qident n, List.map gen_type a)
  | TyVar (TypeVariable (n, _)) ->
     CNamedType (gen_ident n, [])


(* Expressions *)

let union_type_name = function
  | NamedType (n, _, _) ->
     n
  | _ ->
     err "Internal error: Union is not a named type?"

(* Given the name of a union type, returns the name of the
   union's tag enum name. *)
let union_tag_enum_name (n: qident): string =
  (gen_qident n) ^ "_Tag"


(* Given a union type and the name of a case, return the
   value of the tag enum for that case. *)
let union_tag_value (ty: ty) (case_name: identifier): string =
  union_tag_enum_name (union_type_name ty) ^ "::" ^ (gen_ident case_name)

let rec gen_exp (e: texpr): cpp_expr =
  let g = gen_exp in
  match e with
  | TNilConstant ->
     CBool false
  | TBoolConstant b ->
     CBool b
  | TIntConstant i ->
     CInt i
  | TFloatConstant f ->
     CFloat f
  | TStringConstant _ ->
     err "TODO"
  | TVariable (n, _) ->
     CVar (gen_ident n)
  | TFuncall (name, args, _) ->
     CFuncall (gen_qident name, List.map g args)
  | TMethodCall (name, _, args, _) ->
     CFuncall (gen_qident name, List.map g args)
  | TArithmetic (op, lhs, rhs) ->
     CArithmetic (op, g lhs, g rhs)
  | TComparison (op, lhs, rhs) ->
     CComparison (op, g lhs, g rhs)
  | TConjunction (lhs, rhs) ->
     CConjunction (g lhs, g rhs)
  | TDisjunction (lhs, rhs) ->
     CDisjunction (g lhs, g rhs)
  | TNegation e ->
     CNegation (g e)
  | TIfExpression (c, t, f) ->
     CIfExpression (g c, g t, g f)
  | TRecordConstructor (_, values) ->
     CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values)
  | TUnionConstructor (ty, case_name, values) ->
     let args = CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values) in
     CStructInitializer [
         ("tag", CVar (union_tag_value ty case_name));
         ("data", CStructInitializer [(gen_ident case_name, args)])
       ]
