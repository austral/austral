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

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of Type.typespec * ast
                 | Progn of ast list
                 | Funcall of Symbol.symbol * ast list

    (* Toplevel AST *)

    type name = Symbol.symbol
    type param_name = name
    type docstring = string option
    type symbol = Symbol.symbol
    type typespec = Type.typespec

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * name list * docstring * typespec
                     | Defdisjunction of name * name list * docstring * Type.variant_spec list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
         and param = Param of name * typespec
         and method_decl = MethodDecl of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and instance_arg = InstanceArg of name * name Set.set

    (* Transform alpha-renamed AST to this AST *)

    fun transform Alpha.UnitConstant =
        UnitConstant
      | transform (Alpha.BoolConstant b) =
        BoolConstant b
      | transform (Alpha.IntConstant i) =
        IntConstant i
      | transform (Alpha.FloatConstant f) =
        FloatConstant f
      | transform (Alpha.StringConstant s) =
        StringConstant s
      | transform (Alpha.Variable v) =
        Variable v
      | transform (Alpha.Let (var, value, body)) =
        Let (var, transform value, transform body)
      | transform (Alpha.The (ty, exp)) =
        The (ty, transform exp)
      | transform (Alpha.Operation (f, args)) =
        transformOp f (map transform args)
    and transformOp f args =
        if f = au "progn" then
            Progn args
        else if f = au "if" then
            transformCond args
        else if f = au "tuple" then
            TupleCreate (args)
        else if f = au "proj" then
            transformProj args
        else if f = au "allocate" then
            transformAlloc args
        else if f = au "load" then
            transformLoad args
        else if f = au "store" then
            transformStore args
        else
            Funcall (f, args)
    and transformCond [test, cons, alt] =
        Cond (test, cons, alt)
      | transformCond _ =
        raise Fail "Invalid `if` form"
    and transformProj [ast, IntConstant i] =
        TupleProj (ast, Option.valOf (Int.fromString i))
      | transformProj _ =
        raise Fail "Bad `proj` form"
    and transformAlloc [v] =
        Allocate v
      | transformAlloc _ =
        raise Fail "Bad `allocate` form"
    and transformLoad [ptr] =
        Load ptr
      | transformLoad _ =
        raise Fail "Bad `load` form"
    and transformStore [ptr, v] =
        Store (ptr, v)
      | transformStore _ =
        raise Fail "Bad `store` form"

    (* Parse toplevel forms into the toplevel AST *)

    fun transformDefun ((RCST.Symbol name)::params::rt::body) =
        let fun parseParams (RCST.List l) = map parseParam l
              | parseParams _ = raise Fail "defun parameter list must be a list"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad defun parameter"
            and transformExp params rcst =
                let val params' = Set.fromList (map (fn (Param (n, _)) => n) params)
                in
                    transform (Alpha.transform (OAST.transform rcst) params')
                end
            and parseBody ((RCST.StringConstant s)::head::tail) =
                (* When the first element of the body is a string constant, and
                   the remainder of the form is non-empty, we take the string
                   constant to be the docstring. Otherwise, we parse the string
                   constant as an ordinary expression expression *)
                (SOME (CST.escapedToString s), implicitProgn (head::tail))
              | parseBody body =
                (NONE, implicitProgn body)
            and implicitProgn l =
                (RCST.List ((RCST.Symbol (au "progn"))::l))
        in
            let val (docstring, body') = parseBody body
                and params' = parseParams params
            in
                let val body'' = transformExp params' body'
                in
                    Defun (name,
                           params',
                           Type.parseTypespec rt,
                           docstring,
                           body'')
                end
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
                MethodDecl (name,
                            map parseParam params,
                            Type.parseTypespec rt,
                            SOME (CST.escapedToString s))
              | parseMethod (RCST.List [RCST.Symbol name, RCST.List params, rt]) =
                MethodDecl (name,
                            map parseParam params,
                            Type.parseTypespec rt,
                            NONE)
              | parseMethod _ = raise Fail "Bad method definition"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad method parameter"
        in
            let val (docstring, methods) = parseBody body
            in
                Defclass (name, param, docstring, methods)
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
            and parseParam (RCST.Symbol s) = s
              | parseParam _ = raise Fail "Type parameter must be a symbol"
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
        let fun parseBody [RCST.StringConstant s, def] =
                (SOME (CST.escapedToString s), def)
              | parseBody [def] =
                (NONE, def)
              | parseBody _ = raise Fail "Bad deftype form"
            and parseParam (RCST.Symbol s) = s
              | parseParam _ = raise Fail "Type parameter must be a symbol"
            and parseVariants (RCST.List l) =
                map parseVariants l
              | parseVariants _ =
                raise Fail "defdisjunction body must be a list of variants"
            and parseVariant (RCST.List [RCST.Symbol name, tyspec]) =
                Type.VariantSpec (name, SOME (Type.parseTypespec tyspec))
              | parseVariant (RCST.List [RCST.Symbol name]) =
                Type.VariantSpec (name, NONE)
        in
            let val (docstring, variants) = parseBody body
            in
                Defdisjunction (name,
                                map parseParam params,
                                docstring,
                                parseVariants variants)
            end
        end
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
            Module.NicknamesClause (map parseNickElem args)
        end
    and parseUseClause args =
        let fun parseUseElem (RCST.Keyword k) = k
              | parseUseElem _ = raise Fail "Bad :use clause"
        in
            Module.UseClause (map parseUseElem args)
        end
    and parseImportClause ((RCST.Keyword modName)::args) =
        let fun parseImportElem (RCST.Keyword k) = k
              | parseImportElem _ = raise Fail "Bad :import-from clause"
        in
            Module.ImportFromClause (modName, map parseImportElem args)
        end
      | parseImportClause _ =
        raise Fail "Bad :import-from clause"
    and parseExportClause args =
        let fun parseExportElem (RCST.Keyword k) = k
              | parseExportElem _ = raise Fail "Bad export clause"
        in
            Module.ExportClause (map parseExportElem args)
        end
    and parseDocstringClause [RCST.StringConstant s] =
        Module.DocstringClause (CST.escapedToString s)
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
