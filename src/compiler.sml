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

structure Compiler :> COMPILER = struct
    datatype compiler = Compiler of Module.menv * Symbol.module_name

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Ident.mkIdentEx "austral-user")

    type pathname = string

    datatype compilation_unit = FileUnit of pathname
                              | ReplUnit of string

    fun unitForms (FileUnit path) = Parser.parseFile path
      | unitForms (ReplUnit string) = [Parser.parseString string]

    fun declareForm (Compiler (menv, currModuleName)) form =
        let val currModule = case Module.envGet menv currModuleName of
                                 SOME m => m
                               | _ => raise Fail "No module with this name"
        in
            let val resolved = Util.valOf (RCST.resolve menv currModule form)
            in
                let topnode = AST.transformTop resolved
                in
                    raise Fail "Not implemented yet"
                end
            end
        end

    fun declarationPass c (head::tail) =
        let val (rcst, c') = declareForm c head
        in
            let val (forms, c'') = declarationPass c' tail
            in
                (rcst :: forms, c'')
            end
        end
      | declarationPass c nil =
        ([], c)

    fun compileForm _ _ =
        raise Fail "Not implemented yet"

    fun compilationPass c (head::tail) =
        compilationPass (compileForm c head) tail
      | compilationPass c nil =
        c

    fun compileUnit c u =
        let val (forms, c') = (declarationPass c (unitForms u))
        in
            compilationPass c' forms
        end
end
