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
    (* Expression AST *)

    datatype ast = IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of string
                 | Let of string * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Cast of Type.typespec * ast
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
          | escapeChar c = str c

        fun escapeVariable (Symbol.Var (sym, i)) =
            (escapeSymbol sym) ^ "_" ^ (Int.toString i)
    end

    (* Toplevel AST *)

    type typespec = Type.typespec

    datatype top_ast = DefunConcrete of string * (string * typespec) list * typespec * ast

    (* Transform expression AST *)

    fun transform (AST.IntConstant i) =
        IntConstant i
      | transform (AST.FloatConstant f) =
        FloatConstant f
      | transform (AST.StringConstant s) =
        StringConstant s
      | transform (AST.Variable v) =
        Variable (escapeVariable v)
      | transform (AST.Let (var, value, body)) =
        Let (escapeVariable var, transform value, transform body)
      | transform (AST.Cond (test, cons, alt)) =
        Cond (transform test, transform cons, transform alt)
      | transform (AST.TupleCreate exps) =
        TupleCreate (map transform exps)
      | transform (AST.TupleProj (tup, idx)) =
        TupleProj (transform tup, idx)
      | transform (AST.Allocate v) =
        Allocate (transform v)
      | transform (AST.Load ptr) =
        Load (transform ptr)
      | transform (AST.Store (ptr, v)) =
        Store (transform ptr, transform v)
      | transform (AST.The (ty, exp)) =
        Cast (ty, transform exp)
      | transform (AST.Progn exps) =
        Progn (map transform exps)
      | transform (AST.Funcall (f, args)) =
        Funcall (escapeSymbol f, map transform args)

    (* Transform top-level AST *)

    fun transformTop (AST.Defun (name, params, rt, _, body)) =
        DefunConcrete (escapeSymbol name,
                       map (fn (n, t) => (escapeSymbol n, t)) params,
                       rt,
                       transform body)
      | transformTop _ =
        raise Fail "transformTop not implemented for this case"
end
