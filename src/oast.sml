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
    type name = symbol
    type typespec = Type.typespec

    (* Types *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Symbol of symbol
                 | Let of name * ast * ast
                 | Bind of name list * ast * ast
                 | The of typespec * ast
                 | Construct of typespec * name * ast option
                 | Case of ast * variant_case list
                 | ForeignNull of typespec
                 | SizeOf of typespec
                 | AddressOf of name
                 | Operation of name * ast list
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: name }

    type docstring = string option
    type param_name = symbol

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defgeneric of name * param_name list * param list * typespec * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * param_name list * docstring * typespec
                     | Defdisjunction of name * param_name list * docstring * variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
                     | Defcfun of name * string * param list * Function.foreign_arity * typespec * docstring
         and param = Param of name * typespec
         and method_decl = MethodDecl of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and instance_arg = InstanceArg of name * name list
         and variant = Variant of name * typespec option

    (* Parameter list parsing *)

    val au = Symbol.au

    fun parseForeignParamList (RCST.List l) =
        parseForeignParamList' l
      | parseForeignParamList _ =
        raise Fail "defun parameter list must be a list"

    and parseForeignParamList' ((RCST.Symbol keyword)::restp::nil) =
        if keyword = au "&rest" then
            (Function.VariableArity, [parseForeignParam restp])
        else
            raise Fail "Invalid parameter list keyword"
      | parseForeignParamList' (head::tail) =
        let val (arity, tail') = parseForeignParamList' tail
        in
            (arity, (parseForeignParam head) :: tail')
        end
      | parseForeignParamList' nil =
        (Function.FixedArity, nil)

    and parseForeignParam (RCST.List [RCST.Symbol name, ty]) =
        Param (name, Type.parseTypespec ty)
      | parseForeignParam _ =
        raise Fail "Bad defcfun parameter"

    (* Functions *)

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
        else if f = au "bind" then
            transformBind args
        else if f = au "the" then
            transformThe args
        else if f = Symbol.auKer "construct" then
            transformConstruct args
        else if f = au "case" then
            transformCase args
        else if f = Symbol.auCffi "foreign-funcall" then
            transformForeignFuncall args
        else if f = Symbol.auCffi "null-pointer" then
            transformForeignNull args
        else if f = Symbol.auCffi "size-of" then
            transformSizeOf args
        else if f = Symbol.auCffi "address-of" then
            transformAddressOf args
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

    and transformBind ((RCST.List binds)::tup::body) =
        let fun parseBind (RCST.Symbol s) =
                s
              | parseBind _ =
                raise Fail "Not a binding"
        in
          Bind (map parseBind binds,
                transform tup,
                Operation (au "progn", map transform body))
        end
      | transformBind _ =
        raise Fail "Invalid `bind` form"

    and transformThe [ty, exp] =
        The (Type.parseTypespec ty, transform exp)
      | transformThe _ =
        raise Fail "Invalid `the` form"

    and transformConstruct [ty, RCST.Symbol label] =
        Construct (Type.parseTypespec ty, label, NONE)
      | transformConstruct [ty, RCST.Symbol label, value] =
        Construct (Type.parseTypespec ty, label, SOME (transform value))
      | transformConstruct _ =
        raise Fail "Invalid `construct` form"

    and transformCase (exp::rest) =
        Case (transform exp,
              map transformCaseVariant rest)
      | transformCase _ =
        raise Fail "Invalid `case` form"

    and transformCaseVariant (RCST.List (name::body)) =
        VariantCase (parseCaseName name,
                     Operation (au "progn", map transform body))
      | transformCaseVariant _ =
        raise Fail "Invalid `case` form"

    and parseCaseName (RCST.Symbol label) =
        NameOnly label
      | parseCaseName (RCST.List [RCST.Symbol label, RCST.Symbol var]) =
        NameBinding { casename = label, var = var }
      | parseCaseName _ =
        raise Fail "Invalid case name in a `case` form"

    and transformForeignFuncall ((RCST.StringConstant name)::rt::args) =
        ForeignFuncall (CST.escapedToString name,
                        Type.parseTypespec rt,
                        map transform args)
      | transformForeignFuncall _ = raise Fail "Invalid `foreign-funcall` form"

    and transformForeignNull [tys] =
        ForeignNull (Type.parseTypespec tys)
      | transformForeignNull _ =
        raise Fail "Invalid `null-pointer` form"

    and transformSizeOf [tys] =
        SizeOf (Type.parseTypespec tys)
      | transformSizeOf _ =
        raise Fail "Bad `size-of` form"

    and transformAddressOf [RCST.Symbol name] =
        AddressOf name
      | transformAddressOf _ =
        raise Fail "Invalid `address-of` form"

    (* Parse toplevel forms into the toplevel AST *)

    fun transformTop (RCST.List l) = transformTopList l
      | transformTop _ = raise Fail "Invalid toplevel form"
    and transformTopList ((RCST.Symbol f)::args) = transformT f args
      | transformTopList _ = raise Fail "Invalid toplevel form"
    and transformT f args =
        let val au = Symbol.au
        in
            if f = au "defun" then
                transformDefun args
            else if f = au "defgeneric" then
                transformDefgeneric args
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
            else if f = Symbol.auCffi "defcfun" then
                transformDefcfun args
            else
                raise Fail "Unknown toplevel form"
        end

    and transformDefun ((RCST.Symbol name)::params::rt::body) =
        let fun parseParams (RCST.List l) = map parseParam l
              | parseParams _ = raise Fail "defun parameter list must be a list"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad defun parameter"
            and parseBody ((RCST.StringConstant s)::head::tail) =
                (* When the first element of the body is a string constant, and
                   the remainder of the form is non-empty, we take the string
                   constant to be the docstring. Otherwise, we parse the string
                   constant as an ordinary expression expression *)
                (SOME (CST.escapedToString s), implicitProgn (head::tail))
              | parseBody body =
                (NONE, implicitProgn body)
            and implicitProgn l =
                (RCST.List ((RCST.Symbol (Symbol.au "progn"))::l))
        in
            let val (docstring, body') = parseBody body
                and params' = parseParams params
            in
                let val body'' = transform body'
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

    and transformDefgeneric ((RCST.Symbol name)::typarams::params::rt::body) =
        let fun parseParams (RCST.List l) = map parseParam l
              | parseParams _ = raise Fail "defun parameter list must be a list"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad defun parameter"
            and parseTypeParams (RCST.List l) =
                map parseTypeParam l
              | parseTypeParams _ =
                raise Fail "defgeneric type parameter list must be a list"
            and parseTypeParam (RCST.Symbol s) = s
              | parseTypeParam _ = raise Fail "Type parameter must be a symbol"
            and parseBody ((RCST.StringConstant s)::head::tail) =
                (SOME (CST.escapedToString s), implicitProgn (head::tail))
              | parseBody body =
                (NONE, implicitProgn body)
            and implicitProgn l =
                (RCST.List ((RCST.Symbol (Symbol.au "progn"))::l))
        in
            let val (docstring, body') = parseBody body
                and params' = parseParams params
            in
                let val body'' = transform body'
                in
                    Defgeneric (name,
                                parseTypeParams typarams,
                                params',
                                Type.parseTypespec rt,
                                docstring,
                                body'')
                end
            end
        end
      | transformDefgeneric _ =
        raise Fail "Bad defgeneric form"

    and transformDefclass ((RCST.Symbol name)::(RCST.List [RCST.Symbol param])::body) =
        let fun parseBody ((RCST.StringConstant s)::methods)  =
                (SOME (CST.escapedToString s), parseMethods methods)
              | parseBody methods =
                (NONE, parseMethods methods)
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

    and transformDefinstance ((RCST.Symbol name)::(RCST.List args)::body) =
        let fun parseArgs ((RCST.Symbol tyname)::rest) =
                InstanceArg (tyname,
                             parseTyVars rest)
              | parseArgs _ =
                raise Fail "definstance: bad instance argument"
            and parseTyVars list =
                let fun mapSym (RCST.Symbol s) =
                        s
                      | mapSym _ =
                        raise Fail "definstance: type variable must be a symbol"
                in
                    map mapSym list
                end
            and parseBody ((RCST.StringConstant s)::methods)  =
                (SOME (CST.escapedToString s), parseMethods methods)
              | parseBody methods =
                (NONE, parseMethods methods)
            and parseMethods list =
                map parseMethod list
            and parseMethod (RCST.List ((RCST.Symbol name)::(RCST.List params)::rt::body)) =
                let val params' = map parseParam params
                    and (docstring, body') = parseMethodBody body
                in
                    MethodDef (name,
                               params',
                               Type.parseTypespec rt,
                               docstring,
                               transform body')
                end
              | parseMethod _ =
                raise Fail "definstance: bad method form"
            and parseParam (RCST.List [RCST.Symbol name, ty]) =
                Param (name, Type.parseTypespec ty)
              | parseParam _ = raise Fail "Bad method parameter"
            and parseMethodBody ((RCST.StringConstant s)::head::tail) =
                (SOME (CST.escapedToString s), implicitProgn (head::tail))
              | parseMethodBody body =
                (NONE, implicitProgn body)
            and implicitProgn l =
                (RCST.List ((RCST.Symbol (Symbol.au "progn"))::l))
        in
            let val (docstring, methods) = parseBody body
            in
                Definstance (name,
                             parseArgs args,
                             docstring,
                             methods)
            end
        end
      | transformDefinstance _ = raise Fail "Bad definstance form"

    and transformDeftype ((RCST.Symbol name)::(RCST.List params)::body) =
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

    and transformDefdisjunction ((RCST.Symbol name)::(RCST.List params)::body) =
        let fun parseBody ((RCST.StringConstant s)::def) =
                (SOME (CST.escapedToString s), def)
              | parseBody def =
                (NONE, def)
            and parseParam (RCST.Symbol s) = s
              | parseParam _ = raise Fail "Type parameter must be a symbol"
            and parseVariant (RCST.List [RCST.Symbol name, tyspec]) =
                Variant (name, SOME (Type.parseTypespec tyspec))
              | parseVariant (RCST.List [RCST.Symbol name]) =
                Variant (name, NONE)
              | parseVariant _ =
                raise Fail "defdisjunction: bad variant definition"
        in
            let val (docstring, variants) = parseBody body
            in
                Defdisjunction (name,
                                map parseParam params,
                                docstring,
                                map parseVariant variants)
            end
        end
      | transformDefdisjunction _ = raise Fail "Bad defdisjunction form"

    and transformDeftemplate ((RCST.Symbol name)::body) =
        raise Fail "deftemplate not implemented"
      | transformDeftemplate _ = raise Fail "Bad deftemplate form"

    and transformDefSymbolMacro [RCST.Symbol name, expansion, RCST.StringConstant docstring] =
        DefineSymbolMacro (name, expansion, SOME (CST.escapedToString docstring))
      | transformDefSymbolMacro [RCST.Symbol name, expansion] =
        DefineSymbolMacro (name, expansion, NONE)
      | transformDefSymbolMacro _ = raise Fail "Bad define-symbol-macro form"

    and transformDefmodule ((RCST.Symbol name)::clauses) =
        let val clauses = map parseClause clauses
        in
            Defmodule (Symbol.symbolName name, clauses)
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

    and transformInModule [RCST.Keyword moduleName] =
        InModule moduleName
      | transformInModule _ =
        raise Fail "Bad in-module form"

    and transformDefcfun ((RCST.List [RCST.Symbol name,
                                      RCST.StringConstant rawname])
                          ::params
                          ::rt
                          ::nil) =
        let val (arity, params') = parseForeignParamList params
        in
            Defcfun (name,
                     CST.unescapeString rawname,
                     params',
                     arity,
                     Type.parseTypespec rt,
                     NONE)
        end
      | transformDefcfun _ =
        raise Fail "Invalid defcfun form"

end
