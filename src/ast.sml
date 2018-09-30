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
                  | Let0 of Symbol.symbol * ast0 * ast0
                  | The0 of RCST.rcst * ast0
                  | Operation0 of Symbol.symbol * ast0 list

    fun transform0 (RCST.IntConstant i) = IntConstant0 i
      | transform0 (RCST.FloatConstant f) = FloatConstant0 f
      | transform0 (RCST.StringConstant s) = StringConstant0 s
      | transform0 (RCST.Symbol s) = Symbol0 s
      | transform0 (RCST.Keyword s) = raise Fail "Keywords not allowed in expressions"
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
        Let0 (var, transform0 v, Operation0 (au "progn", map transform0 body))
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
                                   ^ (Ident.identString (Symbol.symbolName s))
                                   ^ "'")

    fun alphaRename _ (IntConstant0 s) = IntConstant s
      | alphaRename _ (FloatConstant0 f) = FloatConstant f
      | alphaRename _ (StringConstant0 s) = StringConstant s
      | alphaRename s (Symbol0 name) = Variable (lookup s name)
      | alphaRename s (Let0 (var, value, body)) =
        let val fresh = freshVar var
          in
              let val s' = (var, fresh) :: s
              in
                  let val body' = alphaRename s' body
                  in
                      Let (Binding (fresh, alphaRename s value), body')
                  end
              end
        end
      | alphaRename s (The0 (ty, exp)) = The (ty, alphaRename s exp)
      | alphaRename s (Operation0 (f, args)) = Operation (f, map (alphaRename s) args)

    fun transform rcst =
        let
        in
            resetCount ();
            alphaRename [] (transform0 rcst)
        end

    (* Toplevel AST *)

    type name = Symbol.symbol
    type docstring = string option
    type symbol = Symbol.symbol
    type typespec = Type.typespec

    datatype top_ast = Defun of Function.func * ast
                     | Defclass of Function.typeclass
                     | Definstance of Function.instance
                     | Deftype of name * Type.param list * docstring * typespec
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of defmodule_clause list
                     | InModule of Symbol.symbol_name
         and disjunction_case = DisjCase of name * typespec option
         and defmodule_clause = NicknamesClause of (Symbol.symbol_name * Symbol.module_name) list
                              | UseClause of Symbol.module_name list
                              | ImportFromClause of Symbol.module_name * (Symbol.symbol_name list)
                              | ExportClause of Symbol.symbol_name list
                              | DocstringClause of string


    (* Parse toplevel forms into the toplevel AST *)

    fun transformDefun ((RCST.Symbol name)::params::rt::body) =
        let fun parseParams (RCST.List l) = map parseParam l
              | parseParams _ = raise Fail "defun parameter list must be a list"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Function.Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad defun parameter"
            and parseBody ((RCST.StringConstant s)::head::tail) =
                (* When the first element of the body is a string constant, and
                   the remainder of the form is non-empty, we take the string
                   constant to be the docstring. Otherwise, we parse the string
                   constant as a regular expression *)
                (SOME (CST.escapedToString s), transform (implicitProgn (head::tail)))
              | parseBody body =
                (NONE, transform (implicitProgn body))
            and implicitProgn l =
                (RCST.List ((RCST.Symbol (au "progn"))::l))
        in
            let val (docstring, body') = parseBody body
            in
                Defun (Function.Function (name,
                                          parseParams params,
                                          Type.parseTypespec rt,
                                          docstring),
                       body')
            end
        end
      | transformDefun _ = raise Fail "Bad defun form"

    fun transformDefclass ((RCST.Symbol name)::(RCST.List [RCST.Symbol param])::body) =
        let fun parseBody [RCST.StringConstant s, RCST.List methods]  =
                (SOME (CST.escapedToString s), parseMethods methods)
              | parseBody [RCST.List methods] = (NONE, parseMethods methods)
              | parseBody _ = raise Fail "Bad defclass form"
            and parseMethods l = map parseMethod l
            and parseMethod (RCST.List [RCST.Symbol name, RCST.List params, rt, RCST.StringConstant s]) =
                Function.MethodDecl (name,
                                     map parseParam params,
                                     Type.parseTypespec rt,
                                     SOME (CST.escapedToString s))
              | parseMethod (RCST.List [RCST.Symbol name, RCST.List params, rt]) =
                Function.MethodDecl (name,
                                     map parseParam params,
                                     Type.parseTypespec rt,
                                     NONE)
              | parseMethod _ = raise Fail "Bad method definition"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Function.Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad method parameter"
        in
            let val (docstring, methods) = parseBody body
            in
                Defclass (Function.Typeclass (name, param, docstring, methods))
            end
        end
      | transformDefclass _ = raise Fail "Bad defclass form"

    fun transformDefinstance ((RCST.Symbol name)::(RCST.List [arg])::body) =
        raise Fail "definstance not implemented"
      | transformDefinstance _ = raise Fail "Bad definstance form"

    fun transformDeftype ((RCST.Symbol name)::(RCST.List params)::body) =
        let fun parseBody [RCST.StringConstant s, def] =
                (SOME (CST.escapedToString s), def)
              | parseBody [def] =
                (NONE, def)
              | parseBody _ = raise Fail "Bad deftype form"
            and parseParam (RCST.Symbol s) = Type.TypeParam s
              | parseParam _ = raise Fail "Bad type parameter"
        in
            let val (docstring, ty) = parseBody body
            in
                Deftype (name,
                         map parseParam params,
                         docstring,
                         Type.parseTypespec ty)
            end
        end
      | transformDeftype _ = raise Fail "Bad deftype form"

    fun transformDefdisjunction ((RCST.Symbol name)::(RCST.List params)::body) =
        raise Fail "defdisjunction not implemented"
      | transformDefdisjunction _ = raise Fail "Bad defdisjunction form"

    fun transformDeftemplate ((RCST.Symbol name)::body) =
        raise Fail "deftemplate not implemented"
      | transformDeftemplate _ = raise Fail "Bad deftemplate form"

    fun transformDefSymbolMacro [RCST.Symbol name, expansion, RCST.StringConstant docstring] =
        DefineSymbolMacro (name, expansion, SOME (CST.escapedToString docstring))
      | transformDefSymbolMacro [RCST.Symbol name, expansion] =
        DefineSymbolMacro (name, expansion, NONE)
      | transformDefSymbolMacro _ = raise Fail "Bad define-symbol-macro form"

    fun transformDefmodule ((RCST.Symbol name)::clauses) =
        let val clauses = map parseClause clauses
        in
            raise Fail "Not implemented"
        end
      | transformDefmodule _ =
        raise Fail "Bad defmodule form"
    and parseClause (RCST.List ((RCST.Keyword name)::rest)) =
        parseClauseInner name rest
      | parseClause _ = raise Fail "Bad defmodule clause"
    and parseClauseInner name args =
        let fun key s = Ident.mkIdentEx s
        in
            if name = key "nicknames" then
                parseNicknamesClause args
            else if name = key "use" then
                parseUseClause args
            else if name = key "import-from" then
                parseImportClause args
            else if name = key "export" then
                parseExportClause args
            else if name = key "documentation" then
                parseDocstringClause args
            else
                raise Fail "Unknown export clause"
        end
    and parseNicknamesClause args =
        let fun parseNickElem (RCST.List [RCST.Keyword nick, RCST.Keyword modName]) = (nick, modName)
              | parseNickElem _ = raise Fail "Bad :nicknames clause"
        in
            NicknamesClause (map parseNickElem args)
        end
    and parseUseClause args =
        let fun parseUseElem (RCST.Keyword k) = k
              | parseUseElem _ = raise Fail "Bad :use clause"
        in
            UseClause (map parseUseElem args)
        end
    and parseImportClause ((RCST.Keyword modName)::args) =
        let fun parseImportElem (RCST.Keyword k) = k
              | parseImportElem _ = raise Fail "Bad :import-from clause"
        in
            ImportFromClause (modName, map parseImportElem args)
        end
      | parseImportClause _ =
        raise Fail "Bad :import-from clause"
    and parseExportClause args =
        let fun parseExportElem (RCST.Keyword k) = k
              | parseExportElem _ = raise Fail "Bad export clause"
        in
            ExportClause (map parseExportElem args)
        end
    and parseDocstringClause [RCST.StringConstant s] =
        DocstringClause (CST.escapedToString s)
      | parseDocstringClause _ =
        raise Fail "Bad docstring clause"

    fun transformInModule [RCST.Keyword moduleName] =
        InModule moduleName
      | transformInModule _ =
        raise Fail "Bad in-module form"

    fun transformTop (RCST.List l) = transformTopList l
      | transformTop _ = raise Fail "Invalid toplevel form"
    and transformTopList ((RCST.Symbol f)::args) = transformT f args
      | transformTopList _ = raise Fail "Invalid toplevel form"
    and transformT f args =
        if f = au "defun" then
            transformDefun args
        else if f = au "defclass" then
            transformDefclass args
        else if f = au "definstance" then
            transformDefinstance args
        else if f = au "deftype" then
            transformDeftype args
        else if f = au "defdisjunction" then
            transformDefdisjunction args
        else if f = au "deftemplate" then
            transformDeftemplate args
        else if f = au "define-symbol-macro" then
            transformDefSymbolMacro args
        else if f = au "defmodule" then
            transformDefmodule args
        else if f = au "in-module" then
            transformInModule args
        else
            raise Fail "Unknown toplevel form"
end
