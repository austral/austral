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
            "auto_" ^ (Int.toString (!count))
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
                 | Allocate of ast * ty
                 | Load of ast
                 | Store of ast * ast
                 | Cast of ty * ast
                 | Construct of ty * Symbol.symbol * ast option
                 | Case of ast * (name * ast) list * ty
                 | SizeOf of ty
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
                     | ToplevelProgn of top_ast list
         and param = Param of name * ty

    (* Transform expression AST *)

    fun transform TAst.UnitConstant =
        BoolConstant false
      | transform (TAst.BoolConstant b) =
        BoolConstant b
      | transform (TAst.IntConstant (i, _)) =
        IntConstant i
      | transform (TAst.FloatConstant (f, _)) =
        FloatConstant f
      | transform (TAst.StringConstant s) =
        StringConstant s
      | transform (TAst.Variable (v, _)) =
        Variable (escapeVariable v)
      | transform (TAst.Let (var, value, body)) =
        Let (escapeVariable var, TAst.typeOf value, transform value, transform body)
      | transform (TAst.Bind (vars, tup, body)) =
        (* At the TAST->HIR boundary we map bindings to a chain of let
           expressions where each variable is bound to a tuple projection *)
        let val tupTy = TAst.typeOf tup
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
      | transform (TAst.Cond (test, cons, alt)) =
        Cond (transform test, transform cons, transform alt, TAst.typeOf cons)
      | transform (TAst.ArithOp (kind, oper, lhs, rhs)) =
        let val lhs' = transform lhs
            and rhs' = transform rhs
        in
            case kind of
                Arith.Modular => IntArithOp (oper, lhs', rhs')
              | Arith.Checked => transformCheckedArithOp oper lhs' rhs'
              | Arith.Saturation => transformSaturationArithOp oper lhs' rhs'
              | Arith.Float => FloatArithOp (oper, lhs', rhs')
        end
      | transform (TAst.TupleCreate exps) =
        TupleCreate (map transform exps)
      | transform (TAst.TupleProj (tup, idx)) =
        TupleProj (transform tup, idx)
      | transform (TAst.Allocate v) =
        Allocate (transform v, TAst.typeOf v)
      | transform (TAst.Load ptr) =
        Load (transform ptr)
      | transform (TAst.Store (ptr, v)) =
        Store (transform ptr, transform v)
      | transform (TAst.The (ty, exp)) =
        Cast (ty, transform exp)
      | transform (TAst.Construct (ty, label, exp)) =
        Construct (ty, label, Option.map transform exp)
      | transform (TAst.Case (exp, variants, ty)) =
        (* In the TAST->HIR stage, we move case bindings to their own let *)
        let val temp = gensym ()
        in
            let fun mapVariant (TAst.VariantCase (TAst.NameOnly name, body)) =
                    (escapeSymbol name, transform body)
                  | mapVariant _ =
                    raise Fail "not implemented"
            in
                Let (temp,
                     TAst.typeOf exp,
                     transform exp',
                     Case (Variable temp,
                           map mapVariant variants,
                           ty))
            end
        end
      | transform (TAst.ForeignFuncall (name, rt, args)) =
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
      | transform (TAst.ForeignNull ty) =
        NullConstant
      | transform (TAst.SizeOf ty) =
        SizeOf ty
      | transform (TAst.Seq (a, b)) =
        Seq (transform a, transform b)
      | transform (TAst.Funcall (f, tyargs, args, _)) =
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

    fun transformTop (TAst.Defun (name, params, rt, _, body)) =
        Defun (escapeSymbol name,
               mapParams params,
               rt,
               transform body)
      | transformTop (TAst.Defgeneric (name, typarams, params, rt, _, body)) =
        Defgeneric (escapeSymbol name,
                    mapTypeParams typarams,
                    mapParams params,
                    rt,
                    transform body)
      | transformTop (TAst.Defclass _) =
        (* Defclass declarations don't need to be compiled, all the actual work
           is done in instance declarations *)
        ToplevelProgn []
      | transformTop (TAst.Definstance (_, _, _, methods)) =
        (* The compilation strategy for instances is each method is compiled to
           a standalone generic function *)
        (* FIXME: definstance methods should be compiled to generic functions *)
        ToplevelProgn (map transformMethod methods)
      | transformTop (TAst.Deftype (name, params, _, ty)) =
        Deftype (escapeSymbol name,
                 mapTypeParams params,
                 ty)
      | transformTop (TAst.Defdisjunction (name, params, _, variants)) =
        Defdisjunction (escapeSymbol name,
                        mapTypeParams params,
                        variants)
      | transformTop (TAst.Deftemplate _) =
        ToplevelProgn []
      | transformTop (TAst.DefineSymbolMacro _) =
        ToplevelProgn []
      | transformTop (TAst.Defmodule _) =
        ToplevelProgn []
      | transformTop (TAst.InModule _) =
        ToplevelProgn []

      and mapParams params =
          map (fn (TAst.Param (n, t)) => Param (escapeVariable n, t))
              params

    and mapTypeParams typarams =
        map (fn (Type.TypeParam n) => escapeSymbol n)
            (OrderedSet.toList typarams)

    and transformMethod (TAst.MethodDef (name, params, rt, _, body)) =
        Defun (escapeSymbol name,
               map (fn (TAst.Param (n, t)) => Param (escapeVariable n, t)) params,
               rt,
               transform body)
end
