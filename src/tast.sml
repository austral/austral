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
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | Progn of ast list
                 | Funcall of Symbol.symbol * ast list * ty

    type name = string

    type name = string
    type docstring = string option

    datatype top_ast = Defun of name * param list * ty * docstring * ast
         and param = Param of name * ty

    local
        open Type
    in
        fun typeOf UnitConstant = Unit
          | typeOf (BoolConstant _) = Bool
          | typeOf (IntConstant (_, t)) = t
          | typeOf (FloatConstant (_, t)) = t
          | typeOf (StringConstant _) = raise Fail "not impemented"
          | typeOf (Variable (_, t)) = t
          | typeOf (Let (_, _, b)) = typeOf b
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

    fun bindType (Binding (t, _)) = t

    type bindings = (Symbol.variable, binding) Map.map

    (* Context data for the augment function *)

    datatype context = Context of bindings * Type.tenv * Function.fenv

    fun mkContext b t f = Context (b, t, f)

    fun ctxBindings (Context (b, _, _)) = b
    fun ctxTenv (Context (_, t, _)) = t
    fun ctxFenv (Context (_, _, f)) = f

    (* Augment AST with type information *)

    local
        open Type
    in
        val defaultIntType = Integer (Signed, Int32)
        val defaultFloatType = Float Double

        fun augment AST.UnitConstant _ = UnitConstant
          | augment (AST.BoolConstant b) _ = BoolConstant b
          | augment (AST.IntConstant i) _ = IntConstant (i, defaultIntType)
          | augment (AST.FloatConstant f) _ = FloatConstant (f, defaultFloatType)
          | augment (AST.StringConstant s) _ = StringConstant s
          | augment (AST.Variable name) c =
            (case (Map.get (ctxBindings c) name) of
                 SOME bind => Variable (name, bindType bind)
               | NONE => raise Fail ("No such variable"))
          | augment (AST.Let (name, v, body)) c =
            let val v' = augment v c
            in
                let val s' = Map.iadd (ctxBindings c)
                                      (name, (Binding (typeOf v', Mutable)))
                in
                    Let (name,
                         v',
                         augment body (mkContext s' (ctxTenv c) (ctxFenv c)))
                end
            end
          | augment (AST.Cond (test, cons, alt)) c =
            let val test' = augment test c
                and cons' = augment cons c
                and alt'  = augment alt c
            in
                if (typeOf test') <> Bool then
                    raise Fail "The test in an `if` must be of boolean type"
                else
                    if (typeOf cons') <> (typeOf alt') then
                        raise Fail "The consequent and the alternate must have the same type"
                    else
                        Cond (test', cons', alt')
            end
          | augment (AST.TupleCreate exps) c =
            TupleCreate (map (fn e => augment e c) exps)
          | augment (AST.TupleProj (exp, i)) c =
            TupleProj (augment exp c, i)
          | augment (AST.Allocate v) c =
            Allocate (augment v c)
          | augment (AST.Load e) c =
            let val e' = augment e c
            in
                case (typeOf e') of
                    Pointer t => Load e'
                  | _ => raise Fail "load: not a pointer"
            end
          | augment (AST.Store (p, v)) c =
            let val p' = augment p c
                and v' = augment v c
            in
                case (typeOf p') of
                    Pointer t => let val ty = typeOf v'
                                 in
                                     if ty = t then
                                         Store (p', v')
                                     else
                                         raise Fail "store: type mismatch"
                                 end
                  | _ => raise Fail "store: first argument must be a pointer"
            end
          | augment (AST.The (typespec, exp)) c =
            let val tenv = ctxTenv c
            in
                The (resolve tenv typespec, augment exp c)
            end
          | augment (AST.Progn exps) c =
            Progn (map (fn a => augment a c) exps)
          | augment (AST.Funcall _) _ =
            raise Fail "funcall not implemented"
    end
end
