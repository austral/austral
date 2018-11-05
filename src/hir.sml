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
    type name = string
    type ty = Type.ty

    (* Generate variables *)

    val count = ref 0;

    fun gensym () =
        let
        in
            count := !count + 1;
            "hir_auto_" ^ (Int.toString (!count))
        end

    (* Expression AST *)

    datatype ast = BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | NullConstant
                 | Negation of ast
                 | Variable of string
                 | Let of string * ty * ast * ast
                 | Cond of ast * ast * ast * ty
                 | IntArithOp of Arith.oper * ast * ast
                 | FloatArithOp of Arith.oper * ast * ast
                 | ComparisonOp of Builtin.comp_op * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Allocate of ast * ty
                 | Load of ast
                 | Store of ast * ast
                 | Cast of ty * ast
                 | Construct of ty * Symbol.symbol * ast option
                 | DisjunctionNth of ast * int
                 | TagEq of ast * int
                 | SizeOf of ty
                 | AddressOf of name
                 | Seq of ast * ast
                 | Funcall of string * ty list * ast list

    local
        open Symbol
    in
        fun escapeSymbol symbol =
            let val module = symbolModuleName symbol
                and name = symbolName symbol
            in
                "_A" ^ (escapeIdent module) ^ "___" ^ (escapeIdent name)
            end
        and escapeIdent i =
            escapeString (Ident.identString i)
        and escapeString s =
            String.concat (map escapeChar (String.explode s))
        and escapeChar #"!" = "_ba"
          | escapeChar #"%" = "_pe"
          | escapeChar #"&" = "_am"
          | escapeChar #"$" = "_do"
          | escapeChar #"#" = "_po"
          | escapeChar #"+" = "_pl"
          | escapeChar #"-" = "_da"
          | escapeChar #"*" = "_mu"
          | escapeChar #"/" = "_di"
          | escapeChar #"<" = "_lt"
          | escapeChar #"=" = "_eq"
          | escapeChar #">" = "_gt"
          | escapeChar #"?" = "_in"
          | escapeChar #"@" = "_at"
          | escapeChar #"\\" = "_bs"
          | escapeChar #"~" = "_ti"
          | escapeChar #"^" = "_ca"
          | escapeChar #"|" = "_pi"
          | escapeChar #"'" = "_qu"
          | escapeChar #"." = "_pe"
          | escapeChar c = str c

        fun escapeVariable (Symbol.Var (sym, i)) =
            (escapeSymbol sym) ^ "_" ^ (Int.toString i)
    end

    (* Toplevel AST *)

    datatype top_ast = Defun of name * param list * ty * ast
                     | Defgeneric of name * name list * param list * ty * ast
                     | Deftype of name * name list * ty
                     | Defdisjunction of name * name list * Type.variant list
                     | DeclareForeign of name * ty list * Function.foreign_arity * ty
                     | ToplevelProgn of top_ast list
         and param = Param of name * ty

    (* Transform expression AST *)

    fun transform TAST.UnitConstant =
        BoolConstant false
      | transform (TAST.BoolConstant b) =
        BoolConstant b
      | transform (TAST.IntConstant (i, _)) =
        IntConstant i
      | transform (TAST.FloatConstant (f, _)) =
        FloatConstant f
      | transform (TAST.StringConstant s) =
        StringConstant s
      | transform (TAST.Variable (v, _)) =
        Variable (escapeVariable v)
      | transform (TAST.Let (var, value, body)) =
        Let (escapeVariable var, TAST.typeOf value, transform value, transform body)
      | transform (TAST.Bind (vars, tup, body)) =
        (* At the TAST->HIR boundary we map bindings to a chain of let
           expressions where each variable is bound to a tuple projection *)
        let val tupTy = TAST.typeOf tup
            and tup' = transform tup
            and body' = transform body
        in
            let fun nthTupTy idx =
                    case tupTy of
                        (Type.Tuple tys) => List.nth (tys, idx)
                      | _ => raise Fail "not a tuple"

                and transformBind (var::rest) tupref body idx =
                    Let (escapeVariable var,
                         nthTupTy idx,
                         TupleProj (Variable tupref, idx),
                         transformBind rest tupref body (idx+1))
                  | transformBind nil _ body _ =
                    body
            in
                let val tupref = gensym ()
                in
                    Let (tupref,
                         tupTy,
                         tup',
                         transformBind vars tupref body' 0)
                end
            end
        end
      | transform (TAST.Cond (test, cons, alt)) =
        Cond (transform test, transform cons, transform alt, TAST.typeOf cons)
      | transform (TAST.ArithOp (kind, oper, lhs, rhs)) =
        let val lhs' = transform lhs
            and rhs' = transform rhs
        in
            case kind of
                Arith.Modular => IntArithOp (oper, lhs', rhs')
              | Arith.Checked => transformCheckedArithOp oper lhs' rhs'
              | Arith.Saturation => transformSaturationArithOp oper lhs' rhs'
              | Arith.Float => FloatArithOp (oper, lhs', rhs')
        end
      | transform (TAST.TupleCreate exps) =
        TupleCreate (map transform exps)
      | transform (TAST.TupleProj (tup, idx)) =
        TupleProj (transform tup, idx)
      | transform (TAST.ArrayLength arr) =
        ArrayLength (transform arr)
      | transform (TAST.ArrayPointer arr) =
        ArrayPointer (transform arr)
      | transform (TAST.Allocate v) =
        Allocate (transform v, TAST.typeOf v)
      | transform (TAST.Load ptr) =
        Load (transform ptr)
      | transform (TAST.Store (ptr, v)) =
        Store (transform ptr, transform v)
      | transform (TAST.The (ty, exp)) =
        Cast (ty, transform exp)
      | transform (TAST.Construct (ty, label, exp)) =
        Construct (ty, label, Option.map transform exp)
      | transform (TAST.Case (exp, variants, ty)) =
        (* In the TAST->HIR stage, we move case bindings to their own let *)
        let val temp = gensym ()
        in
            let fun mapVariant (TAST.VariantCase (TAST.NameOnly name, body)) =
                    (name, transform body)
                  | mapVariant (TAST.VariantCase (TAST.NameBinding {casename, var, ty}, body)) =
                    (casename,
                     Let (escapeVariable var,
                          ty,
                          DisjunctionNth (Variable temp, disjIndex casename),
                          transform body))

                and disjIndex casename =
                    case TAST.typeOf exp of
                        (Type.Disjunction (_, _, variants)) => Option.valOf (Type.posInVariants variants casename)
                      | _ => raise Fail "not a disjunction"

                and processVariants variants =
                    case variants of
                        [(n, body)] => body
                      | ((casename, body)::tail) =>
                        Cond (TagEq (Variable temp, disjIndex casename),
                              body,
                              processVariants tail,
                              ty)
                      | _ => raise Fail "Invalid case expression: empty"
            in
                Let (temp,
                     TAST.typeOf exp,
                     transform exp,
                     processVariants (map mapVariant variants))
            end
        end
      | transform (TAST.ForeignFuncall (name, args, rt)) =
        (* If the function return type is unit, we're calling a function that
           returns void. In which case use a progn to seq call the function,
           and then return the unit constant *)
        let val call = Funcall (name, [], map transform args)
        in
            if rt = Type.Unit then
                Seq (call, BoolConstant false)
            else
                Cast (rt, call)
        end
      | transform (TAST.ForeignNull ty) =
        NullConstant
      | transform (TAST.SizeOf ty) =
        SizeOf ty
      | transform (TAST.AddressOf (var, _)) =
        AddressOf (escapeVariable var)
      | transform (TAST.Cast (ty, exp)) =
        Cast (ty, transform exp)
      | transform (TAST.Seq (a, b)) =
        Seq (transform a, transform b)
      | transform (TAST.ConcreteFuncall (f, args, _)) =
        raise Fail "Not implemented"
      | transform (TAST.Funcall (f, tyargs, args, _)) =
        transformFuncall f tyargs (map transform args)
    and transformCheckedArithOp oper lhs rhs =
        Funcall ("austral_checked_" ^ arithOpName oper, [], [lhs, rhs])
    and transformSaturationArithOp oper lhs rhs =
        Funcall ("austral_saturation_" ^ arithOpName oper, [], [lhs, rhs])
    and arithOpName (Arith.Add) = "add"
      | arithOpName (Arith.Sub) = "sub"
      | arithOpName (Arith.Mul) = "mul"
      | arithOpName (Arith.Div) = "div"
    and transformFuncall s tyargs args =
        let val au = Symbol.au
            and auKer = Symbol.auKer
        in
            (* If the function is a builtin, it will be compiled to something
               particular, or to a funcall with a special name. Otherwise, it's
               a user-defined function, so just escape the symbol. *)
            if s = au "not" then
                transformNegation args
            else if s = auKer "eq" then
                transformCompOp Builtin.EqualTo args
            else if s = auKer "<" then
                transformCompOp Builtin.LessThan args
            else if s = auKer "<=" then
                transformCompOp Builtin.LessThanEq args
            else if s = auKer ">" then
                transformCompOp Builtin.GreaterThan args
            else if s = auKer ">=" then
                transformCompOp Builtin.GreaterThanEq args
            else
                Funcall (escapeSymbol s, tyargs, args)
        end

    and transformNegation [v] =
        Negation v
      | transformNegation _ =
        raise Fail "not: bad arguments"

    and transformCompOp oper [lhs, rhs] =
        ComparisonOp (oper, lhs, rhs)
      | transformCompOp _ _ =
        raise Fail "Comparison builtin: wrong number of arguments"

    (* Transform top-level AST *)

    fun transformTop (TAST.Defun (name, params, rt, _, body)) =
        Defun (escapeSymbol name,
               mapParams params,
               rt,
               transform body)
      | transformTop (TAST.Defgeneric (name, typarams, params, rt, _, body)) =
        Defgeneric (escapeSymbol name,
                    mapTypeParams typarams,
                    mapParams params,
                    rt,
                    transform body)
      | transformTop (TAST.Defclass _) =
        (* Defclass declarations don't need to be compiled, all the actual work
           is done in instance declarations *)
        ToplevelProgn []
      | transformTop (TAST.Definstance (_, _, _, methods)) =
        (* The compilation strategy for instances is each method is compiled to
           a standalone generic function *)
        (* FIXME: definstance methods should be compiled to generic functions *)
        ToplevelProgn (map transformMethod methods)
      | transformTop (TAST.Deftype (name, params, _, ty)) =
        Deftype (escapeSymbol name,
                 mapTypeParams params,
                 ty)
      | transformTop (TAST.Defdisjunction (name, params, _, variants)) =
        Defdisjunction (escapeSymbol name,
                        mapTypeParams params,
                        variants)
      | transformTop (TAST.Deftemplate _) =
        ToplevelProgn []
      | transformTop (TAST.DefineSymbolMacro _) =
        ToplevelProgn []
      | transformTop (TAST.Defmodule _) =
        ToplevelProgn []
      | transformTop (TAST.InModule _) =
        ToplevelProgn []
      | transformTop (TAST.Defcfun (_, rawname, params, arity, rt, _)) =
        DeclareForeign (rawname,
                        map (fn (TAST.Param (_, ty)) => ty) params,
                        arity,
                        rt)

      and mapParams params =
          map (fn (TAST.Param (n, t)) => Param (escapeVariable n, t))
              params

    and mapTypeParams typarams =
        map (fn (Type.TypeParam n) => escapeSymbol n)
            (OrderedSet.toList typarams)

    and transformMethod (TAST.MethodDef (name, params, rt, _, body)) =
        Defun (escapeSymbol name,
               map (fn (TAST.Param (n, t)) => Param (escapeVariable n, t)) params,
               rt,
               transform body)
end
