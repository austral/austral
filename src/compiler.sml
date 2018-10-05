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
    type code = string

    datatype compiler = Compiler of Module.menv * Type.tenv * Function.fenv * Symbol.module_name * code list

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Type.defaultTenv,
                                  Function.emptyFenv,
                                  Ident.mkIdentEx "austral-user",
                                  [])

    fun compilerMenv (Compiler (menv, _, _, _, _)) =
        menv

    fun compilerTenv (Compiler (_, tenv, _, _, _)) =
        tenv

    fun compilerFenv (Compiler (_, _, fenv, _, _)) =
        fenv

    fun currentModule (Compiler (menv, _, _, modName, _)) =
        Option.valOf (Module.envGet menv modName)

    type pathname = string

    datatype compilation_unit = FileUnit of pathname
                              | ReplUnit of string

    fun unitForms (FileUnit path) = Parser.parseFile path
      | unitForms (ReplUnit string) = [Parser.parseString string]

    local
        open TAst
    in
        fun declareForm compiler form =
            let val resolved = Util.valOf (RCST.resolve (compilerMenv compiler)
                                                        (currentModule compiler)
                                                        form)
            in
                let val topnode = AST.transformTop resolved
                in
                    let val typedNode = TAst.augmentTop topnode
                                                        (compilerTenv compiler)
                                                        (compilerFenv compiler)
                    in
                        (typedNode, declareTopForm compiler typedNode)
                    end
                end
            end
        and declareTopForm c (Defun (name, params, rt, docstring, ast)) =
            (* Add a concrete function to the compiler fenv *)
            let val (Compiler (menv, tenv, fenv, currModuleName)) = c
            in
                let val f = Function.Function (name,
                                               map (fn (Param (n, t)) => Function.Param (n, t))
                                                   params,
                                               rt,
                                               docstring)
                in
                    case (Function.addFunction fenv f) of
                        SOME fenv' => Compiler (menv, tenv, fenv', currModuleName)
                      | NONE => raise Fail "Repeat function"
                end
            end
(*          | declareTopForm c (Deftype (name, params, docstring, def)) =
            let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
                and (Compiler (menv, tenv, fenv, module)) = c
            in
                case (Type.addTypeAlias tenv (name, params', def)) of
                    SOME tenv' => Compiler (menv, tenv', fenv, module)
                  | NONE => raise Fail "Duplicate type definition"
            end
          | declareTopForm c (Defdisjunction (name, params, docstring, variants)) =
            let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
                and (Compiler (menv, tenv, fenv, module)) = c
            in
                case (Type.addDisjunction tenv (name, params', variants)) of
                    SOME tenv' => Compiler (menv, tenv', fenv, module)
                  | NONE => raise Fail "Duplicate type definition"
            end
          | declareTopForm c (InModule moduleName) =
            (* Switch current module *)
            let val (Compiler (menv, tenv, fenv, currModuleName)) = c
            in
                let val newModule = case Module.envGet menv moduleName of
                                        SOME m => m
                                      | NONE => raise Fail "in-module: no module with this name"
                in
                    Compiler (menv, tenv, fenv, moduleName)
                end
            end*)
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

    local
        open TAst
    in
        fun compileForm c topNode =
            let val hirTop = HIR.transformTop topNode
            in
                let val mir = MIR.transformTop hirTop
                in
                    let val cpp = CppBackend.transformTop mir
                    in
                        c
                    end
                end
            end
    end

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
