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

structure TAst :> TAST = struct
    type ty = Type.ty

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ty * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | Progn of ast list
                 | Funcall of Symbol.symbol * ast list * ty

    local
        open Type
    in
        fun typeOf UnitConstant = Unit
          | typeOf (BoolConstant _) = Bool
          | typeOf (IntConstant (_, t)) = t
          | typeOf (FloatConstant (_, t)) = t
          | typeOf (StringConstant _) = raise Fail "not impemented"
          | typeOf (Variable (_, t)) = t
          | typeOf (Let (_, _, _, b)) = typeOf b
          | typeOf (Cond (_, tb, _)) = typeOf tb
          | typeOf (TupleCreate exps) = Tuple (map typeOf exps)
          | typeOf (TupleProj (tup, idx)) =
            (case typeOf tup of
                 Tuple tys => List.nth (tys, idx)
               | _ => raise Fail "Not a tuple")
          | typeOf (Allocate v) = Pointer (typeOf v)
          | typeOf (Load p) =
            (case typeOf p of
                 (Pointer t) => t
               | _ => raise Fail "Not a pointer")
          | typeOf (Store (p, _)) = typeOf p
          | typeOf (The (t, _)) = t
          | typeOf (Progn (x::xs)) = typeOf (List.last (x::xs))
          | typeOf (Progn nil) = Unit
          | typeOf (Funcall (_, _, t)) = t
    end

    (* Bindings *)

    datatype binding = Binding of ty * mutability
         and mutability = Immutable
                        | Mutable

    type bindings = (Ident.ident, binding) Map.map

    (* Context data for the augment function *)

    datatype context = Context of bindings * Type.tenv * Function.fenv

    fun mkContext b t f = Context (b, t, f)

    fun ctxBindings (Context (b, _, _)) = b
    fun ctxTenv (Context (_, t, _)) = t
    fun ctxFenv (Context (_, _, f)) = f

    (* Augment AST with type information *)

    fun augment _ _ = raise Fail "derp"

    (*fun augment AST.ConstUnit = TConstUnit
      | augment (ConstBool b) _ = TConstBool b
      | augment (ConstInt i) _ = TConstInt (i, defaultIntType)
      | augment (ConstString s) _ = TConstString s*)
end
