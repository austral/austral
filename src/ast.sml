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

structure AST :> AST = struct
    (* Utils *)

    fun au name =
        Symbol.mkSymbol (Ident.mkIdentEx "austral",
                         Ident.mkIdentEx name)

    (* Expression AST *)

    datatype ast = IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable
                 | Let of binding * ast
                 | The of RCST.rcst * ast
                 | Operation of Symbol.symbol * ast list
         and binding = Binding of Symbol.variable * ast

    (* Transform RCST to the expression AST *)

    (* Intermediate AST 0 *)
    datatype ast0 = IntConstant0 of string
                  | FloatConstant0 of string
                  | StringConstant0 of CST.escaped_string
                  | Symbol0 of Symbol.symbol
                  | Keyword0 of Symbol.symbol_name
                  | Let0 of (Symbol.symbol * ast0) * ast0
                  | The0 of RCST.rcst * ast0
                  | Operation0 of Symbol.symbol * ast0 list

    fun transform0 (RCST.IntConstant i) = IntConstant0 i
      | transform0 (RCST.FloatConstant f) = FloatConstant0 f
      | transform0 (RCST.StringConstant s) = StringConstant0 s
      | transform0 (RCST.Symbol s) = Symbol0 s
      | transform0 (RCST.Keyword s) = Keyword0 s
      | transform0 (RCST.Splice _) = raise Fail "Splices not allowed in expressions"
      | transform0 (RCST.List l) = transformList0 l
    and transformList0 ((RCST.Symbol f)::args) = transformOp0 f args
      | transformList0 _ = raise Fail "Invalid list form"
    and transformOp0 f args =
        if f = au "let" then
            transformLet0 args
        else if f = au "the" then
            case args of
                [ty, exp] => The0 (ty, transform0 exp)
              | _ => raise Fail "Invalid `the` form"
        else
            Operation0 (f, map transform0 args)
    and transformLet0 ((RCST.List [RCST.List [RCST.Symbol var, v]])::body) =
        (* A let with a single binding *)
        Let0 ((var, transform0 v), Operation0 (au "progn", map transform0 body))
      | transformLet0 ((RCST.List ((RCST.List [RCST.Symbol var, v])::rest))::body) =
        (* A let with at least one binding *)
        let val exp = RCST.List [RCST.Symbol (au "let"),
                                 RCST.List [RCST.List [RCST.Symbol var,
                                                       v]],
                                 RCST.List ((RCST.Symbol (au "let"))::(RCST.List rest)::body)]
        in
            transform0 exp
        end
      | transformLet0 ((RCST.List nil)::body) =
        (* A let with no bindings *)
        Operation0 (au "progn", map transform0 body)
      | transformLet0 _ = raise Fail "Invalid let form"

    (* Alpha renaming *)

    val count = ref 0

    fun freshVar sym =
        let
        in
            count := !count + 1;
            Symbol.Var (sym, !count)
        end

    fun resetCount () =
        count := 0

    type stack = (Symbol.symbol * Symbol.variable) list

    fun lookup ((n,v)::xs) s = if (n = s) then
                                   v
                               else
                                   lookup xs s
      | lookup nil s = raise Fail ("No such variable: '"
                                   ^ (Ident.toString (Symbol.symbolName s))
                                   ^ "'")

    fun alphaRename _ (IntConstant0 s) = IntConstant s
      | alphaRename _ (FloatConstant0 f) = FloatConstant f
      | alphaRename _ (StringConstant0 s) = StringConstant s
      | alphaRename s (Symbol0 name) = Variable (lookup s name)

    fun transform _ = raise Fail "derp"

    (* Toplevel AST *)

    type name = Symbol.symbol
    type docstring = string option
    type symbol = Symbol.symbol
    type typespec = Type.typespec

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defclass of name * symbol * docstring * method list
                     | Definstance of name * typespec * docstring * method_def list
                     | Deftype of name * Type.param list * typespec * docstring
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Module.module
                     | InModule of Symbol.symbol_name
         and param = Param of name * typespec
         and method = Method of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and disjunction_case = DisjCase of name * typespec option

    (* Parse toplevel forms into the toplevel AST *)

    fun transformTop (RCST.List l) = transformTopList l
      | transformTop _ = raise Fail "Invalid toplevel form"
    and transformTopList ((RCST.Symbol f)::args) = transformT f args
      | transformTopList _ = raise Fail "Invalid toplevel form"
    and transformT f args =
        if f = au "defun" then
            transformDefun args
        else if f = au "defclass" then
            transformDefclass args
        else
            raise Fail "Unknown toplevel form"
    and transformDefun ((RCST.Symbol name)::params::rt::body) =
        raise Fail "defun not implemented"
      | transformDefun _ = raise Fail "Bad defun form"
    and transformDefclass ((RCST.Symbol name)::arg::body) =
        raise Fail "defclass not implemented"
      | transformDefclass _ = raise Fail "Bad defclass form"
end
