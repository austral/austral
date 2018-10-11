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
    type ty = Type.ty

    (* Expression AST *)

    datatype ast = BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
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
                 | Progn of ast list
                 | Funcall of string * ast list

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

    type name = string

    datatype top_ast = Defun of name * param list * ty * ast
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
      | transform (TAst.Progn exps) =
        Progn (map transform exps)
      | transform (TAst.Funcall (f, args, _)) =
        transformFuncall f (map transform args)
    and transformCheckedArithOp oper lhs rhs =
        Funcall ("austral_checked_" ^ arithOpName oper, [lhs, rhs])
    and transformSaturationArithOp oper lhs rhs =
        Funcall ("austral_saturation_" ^ arithOpName oper, [lhs, rhs])
    and arithOpName (Arith.Add) = "add"
      | arithOpName (Arith.Sub) = "sub"
      | arithOpName (Arith.Mul) = "mul"
      | arithOpName (Arith.Div) = "div"
    and transformFuncall s args =
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
            else
                Funcall (escapeSymbol s, args)
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
               map (fn (TAst.Param (n, t)) => Param (escapeSymbol n, t)) params,
               rt,
               transform body)
      | transformTop (TAst.Defclass _) =
        ToplevelProgn []
      | transformTop (TAst.Definstance _) =
        raise Fail "definstance to HIR not implemented"
      | transformTop (TAst.Deftype (name, params, _, ty)) =
        Deftype (escapeSymbol name,
                 map escapeSymbol params,
                 ty)
      | transformTop (TAst.Defdisjunction (name, params, _, variants)) =
        Defdisjunction (escapeSymbol name,
                        map escapeSymbol params,
                        variants)
      | transformTop (TAst.Deftemplate _) =
        ToplevelProgn []
      | transformTop (TAst.DefineSymbolMacro _) =
        ToplevelProgn []
      | transformTop (TAst.Defmodule _) =
        ToplevelProgn []
      | transformTop (TAst.InModule _) =
        ToplevelProgn []
end
