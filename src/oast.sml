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

structure OAST :> OAST = struct
    type symbol = Symbol.symbol

    (* Types *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Symbol of symbol
                 | Let of symbol * ast * ast
                 | The of Type.typespec * ast
                 | Operation of symbol * ast list

    type docstring = string option

    (* Functions *)

    val au = Symbol.au

    fun transform (RCST.IntConstant i) = IntConstant i
      | transform (RCST.FloatConstant f) = FloatConstant f
      | transform (RCST.StringConstant s) = StringConstant s
      | transform (RCST.Symbol s) =
        if s = au "nil" then
            UnitConstant
        else if s = au "false" then
            BoolConstant false
        else if s = au "true" then
            BoolConstant true
        else
            Symbol s
      | transform (RCST.Keyword s) = raise Fail "Keywords not allowed in expressions"
      | transform (RCST.Splice _) = raise Fail "Splices not allowed in expressions"
      | transform (RCST.List l) = transformList l
    and transformList ((RCST.Symbol f)::args) = transformOp f args
      | transformList _ = raise Fail "Invalid list form"
    and transformOp f args =
        if f = au "let" then
            transformLet args
        else if f = au "the" then
            case args of
                [ty, exp] => The (Type.parseTypespec ty, transform exp)
              | _ => raise Fail "Invalid `the` form"
        else
            Operation (f, map transform args)
    and transformLet ((RCST.List [RCST.List [RCST.Symbol var, v]])::body) =
        (* A let with a single binding *)
        Let (var, transform v, Operation (au "progn", map transform body))
      | transformLet ((RCST.List ((RCST.List [RCST.Symbol var, v])::rest))::body) =
        (* A let with at least one binding *)
        let val exp = RCST.List [RCST.Symbol (au "let"),
                                 RCST.List [RCST.List [RCST.Symbol var,
                                                       v]],
                                 RCST.List ((RCST.Symbol (au "let"))::(RCST.List rest)::body)]
        in
            transform exp
        end
      | transformLet ((RCST.List nil)::body) =
        (* A let with no bindings *)
        Operation (au "progn", map transform body)
      | transformLet _ = raise Fail "Invalid let form"
end
