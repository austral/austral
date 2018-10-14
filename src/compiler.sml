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

structure Compiler : COMPILER = struct
    type code = string

    datatype compiler = Compiler of Module.menv * Type.tenv * Function.fenv * Symbol.module_name * code

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Type.defaultTenv,
                                  Function.defaultFenv,
                                  Ident.mkIdentEx "austral-user",
                                  CppPrelude.prelude)

    fun compilerMenv (Compiler (menv, _, _, _, _)) =
        menv

    fun compilerTenv (Compiler (_, tenv, _, _, _)) =
        tenv

    fun compilerFenv (Compiler (_, _, fenv, _, _)) =
        fenv

    fun currentModule (Compiler (menv, _, _, modName, _)) =
        Option.valOf (Module.envGet menv modName)

    fun compilerCode (Compiler (_, _, _, _, code)) =
        code

    type pathname = string

    datatype compilation_unit = FileUnit of pathname
                              | ReplUnit of string

    fun unitForms (FileUnit path) = Parser.parseFile path
      | unitForms (ReplUnit string) = [Parser.parseString string]

    fun declareForm compiler form =
        let val resolved = Util.valOf (RCST.resolve (compilerMenv compiler)
                                                    (currentModule compiler)
                                                    form)
        in
            let val topNode = AST.transformTop (Alpha.transformTop (OAST.transformTop resolved))
            in
                (topNode, declareTopForm compiler topNode)
            end
        end
    and declareTopForm c (AST.Defun (name, params, rt, docstring, ast)) =
        (* Add a concrete function to the compiler fenv *)
        let val (Compiler (menv, tenv, fenv, currModuleName, code)) = c
        in
            let val f = Function.Function (name,
                                           map (fn (AST.Param (n, t)) => Function.Param (Symbol.varSymbol n,
                                                                                         Type.resolve tenv t))
                                               params,
                                           Type.resolve tenv rt,
                                           docstring)
            in
                case (Function.addFunction fenv f) of
                    SOME fenv' => Compiler (menv, tenv, fenv', currModuleName, code)
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (AST.Deftype (name, params, docstring, def)) =
        let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
            and (Compiler (menv, tenv, fenv, module, code)) = c
        in
            case (Type.addTypeAlias tenv (name, params', Type.resolve tenv def)) of
                SOME tenv' => Compiler (menv, tenv', fenv, module, code)
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (AST.Defdisjunction (name, params, docstring, variants)) =
        let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
            and (Compiler (menv, tenv, fenv, module, code)) = c
        in
            let fun mapVariant (AST.Variant (name, SOME tys)) =
                    Type.Variant (name, SOME (Type.resolve tenv tys))
                  | mapVariant (AST.Variant (name, NONE)) =
                    Type.Variant (name, NONE)
            in
                case (Type.addDisjunction tenv (name, params', map mapVariant variants)) of
                    SOME tenv' => Compiler (menv, tenv', fenv, module, code)
                  | NONE => raise Fail "Duplicate type definition"
            end
        end
      | declareTopForm c (AST.Defmodule (name, clauses)) =
        let val (Compiler (menv, tenv, fenv, moduleName, code)) = c
        in
            let val module = Module.resolveModule menv name clauses
            in
                case Module.envGet menv name of
                    SOME _ => raise Fail "Duplicate module definition"
                  | NONE => let val menv' = Module.addModule menv module
                            in
                                Compiler (menv', tenv, fenv, moduleName, code)
                            end
            end
        end
      | declareTopForm c (AST.InModule moduleName) =
        (* Switch current module *)
        let val (Compiler (menv, tenv, fenv, currModuleName, code)) = c
        in
            let val newModule = case Module.envGet menv moduleName of
                                    SOME m => m
                                  | NONE => raise Fail "in-module: no module with this name"
            in
                Compiler (menv, tenv, fenv, moduleName, code)
            end
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
        let val typedNode = TAst.augmentTop topNode
                                            (compilerTenv c)
                                            (compilerFenv c)
        in
            let val hirTop = HIR.transformTop typedNode
            in
                let val mir = MIR.transformTop hirTop
                in
                    let val cpp = CppBackend.transformTop mir
                    in
                        let val cppStr = CppAst.renderTop cpp
                            and (Compiler (menv, tenv, fenv, currMod, code)) = c
                        in
                            Compiler (menv, tenv, fenv, currMod, code ^ "\n\n" ^ cppStr)
                        end
                    end
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

    fun compileUnits c (head::tail) =
        compileUnits (compileUnit c head) tail
      | compileUnits c nil =
        c
end
