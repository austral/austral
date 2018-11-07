(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Boreal.

    Boreal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Boreal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Boreal.  If not, see <http://www.gnu.org/licenses/>.
*)

structure HIR :> HIR = struct
    type name = Symbol.symbol

    (* Type System *)

    datatype ty = Unit
                | Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of ty list
                | Pointer of ty
                | StaticArray of ty
                | Disjunction of name * int

    (* Expression AST *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | UnsafeExtractCase of ast * name * ty
                 | ForeignFuncall of string * ast list * ty
                 | ForeignNull of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | ConcreteFuncall of name * ast list * ty
                 | GenericFuncall of name * int * ast list * ty
         and variant_case = VariantCase of name * ast

    fun typeOf UnitConstant =
        Unit
      | typeOf (BoolConstant _) =
        Bool
      | typeOf (IntConstant (_, t)) =
        t
      | typeOf (FloatConstant (_, t)) =
        t
      | typeOf (StringConstant s) =
        StaticArray (Integer (Type.Unsigned, Type.Int8))
      | typeOf (Variable (_, t)) =
        t
      | typeOf (Let (_, _, b)) =
        typeOf b
      | typeOf (Cond (_, tb, _)) =
        typeOf tb
      | typeOf (ArithOp (kind, _, lhs, _)) =
        (case kind of
             Arith.Checked => Tuple [typeOf lhs, Bool]
           | _ => typeOf lhs)
      | typeOf (TupleCreate exps) =
        Tuple (map typeOf exps)
      | typeOf (TupleProj (tup, idx)) =
        (case typeOf tup of
             Tuple tys => List.nth (tys, idx)
           | _ => raise Fail "Not a tuple")
      | typeOf (ArrayLength _) =
        Integer (Type.Unsigned, Type.Int64)
      | typeOf (ArrayPointer arr) =
        (case (typeOf arr) of
             (StaticArray ty) => Pointer ty
           | _ => raise Fail "Invalid type for ArrayPointer")
      | typeOf (Allocate v) =
        Pointer (typeOf v)
      | typeOf (Load p) =
        (case typeOf p of
             (Pointer t) => t
           | _ => raise Fail "Not a pointer")
      | typeOf (Store (p, _)) =
        typeOf p
      | typeOf (Construct (t, _, _)) =
        t
      | typeOf (Case (_, _, t)) =
        t
      | typeOf (UnsafeExtractCase (_, _, t)) =
        t
      | typeOf (SizeOf _) =
        Integer (Type.Unsigned, Type.Int64)
      | typeOf (AddressOf (_, ty)) =
        ty
      | typeOf (Cast (ty, _)) =
        ty
      | typeOf (ForeignFuncall (_, _, rt)) =
        rt
      | typeOf (ForeignNull ty) =
        Pointer ty
      | typeOf (Seq (_, v)) =
        typeOf v
      | typeOf (ConcreteFuncall (_, _, ty)) =
        ty
      | typeOf (GenericFuncall (_, _, _, ty)) =
        ty

    (* Toplevel AST *)

    datatype top_ast = Defun of name * param list * ty * ast
                     | DefunMonomorph of name * param list * ty * ast * int
                     | DeftypeMonomorph of name * ty * int
                     | ToplevelProgn of top_ast list
         and param = Param of Symbol.variable * ty
end
