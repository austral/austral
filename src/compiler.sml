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
    datatype compiler = Compiler of Module.menv * Function.fenv * Symbol.module_name

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Function.emptyFenv,
                                  Ident.mkIdentEx "austral-user")

    type pathname = string

    datatype compilation_unit = FileUnit of pathname
                              | ReplUnit of string

    fun unitForms (FileUnit path) = Parser.parseFile path
      | unitForms (ReplUnit string) = [Parser.parseString string]

    local
        open AST
    in
    fun declareForm compiler form =
        let val (Compiler (menv, _, currModuleName)) = compiler
        in
            let val currModule = case Module.envGet menv currModuleName of
                                     SOME m => m
                                   | _ => raise Fail "No module with this name"
            in
                let val resolved = Util.valOf (RCST.resolve menv currModule form)
                in
                    let val topnode = AST.transformTop resolved
                    in
                        (topnode, declareTopForm compiler topnode)
                    end
                end
            end
        end
    and declareTopForm c (Defun (f, ast)) =
        (* Add a concrete function to the compiler fenv *)
        let val (Compiler (menv, fenv, currModuleName)) = c
        in
            case (Function.addFunction fenv f) of
                SOME fenv' => Compiler (menv, fenv', currModuleName)
              | NONE => raise Fail "Repeat function"
        end
      | declareTopForm c (InModule moduleName) =
        (* Switch current module *)
        let val (Compiler (menv, fenv, currModuleName)) = c
        in
            let val newModule = case Module.envGet menv moduleName of
                                    SOME m => m
                                  | NONE => raise Fail "in-module: no module with this name"
            in
                Compiler (menv, fenv, moduleName)
            end
        end
      | declareTopForm _ _ = raise Fail "Not implemented yet"
    end

    fun declarationPass c (head::tail) =
        let val (node, c') = declareForm c head
        in
            let val (nodes, c'') = declarationPass c' tail
            in
                (node :: nodes, c'')
            end
        end
      | declarationPass c nil =
        ([], c)

    fun compileForm _ _ =
        raise Fail "compileForm Not implemented yet"

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
