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

    datatype compiler = Compiler of Module.menv
                                    * Macro.macroenv
                                    * Type.tenv
                                    * Function.fenv
                                    * Symbol.module_name
                                    * code

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Macro.emptyMacroEnv,
                                  Type.defaultTenv,
                                  Function.defaultFenv,
                                  Ident.mkIdentEx "austral-user",
                                  CppPrelude.prelude)

    fun compilerMenv (Compiler (menv, _, _, _, _, _)) =
        menv

    fun compilerTenv (Compiler (_, _, tenv, _, _, _)) =
        tenv

    fun compilerFenv (Compiler (_, _, _, fenv, _, _)) =
        fenv

    fun currentModule (Compiler (menv, _, _, _, modName, _)) =
        Option.valOf (Module.envGet menv modName)

    fun compilerCode (Compiler (_, _, _, _, _, code)) =
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
                let val dastNode = DAST.transformTop topNode
                                                     (compilerTenv compiler)
                                                     (compilerFenv compiler)
                in
                    (dastNode, declareTopForm compiler dastNode)
                end
            end
        end
    and declareTopForm c (DAST.Defun (name, params, rt, docstring, ast)) =
        (* Add a concrete function to the compiler fenv *)
        let val (Compiler (menv, macenv, tenv, fenv, currModuleName, code)) = c
        in
            let val f = Function.Function (name,
                                           map (fn (DAST.Param (n, t)) => Function.Param (Symbol.varSymbol n, t))
                                               params,
                                           rt,
                                           docstring)
            in
                case (Function.addFunction fenv f) of
                    SOME fenv' => Compiler (menv, macenv, tenv, fenv', currModuleName, code)
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (DAST.Defgeneric (name, typarams, params, rt, docstring, ast)) =
        (* Add a generic function to the compiler fenv *)
        let val (Compiler (menv, macenv, tenv, fenv, currModuleName, code)) = c
        in
            let val gf = Function.GenericFunction (name,
                                                   typarams,
                                                   map (fn (DAST.Param (n, t)) => Function.Param (Symbol.varSymbol n, t))
                                                       params,
                                                   rt,
                                                   docstring)
            in
                case (Function.addGenericFunction fenv gf) of
                    SOME fenv' => Compiler (menv, macenv, tenv, fenv', currModuleName, code)
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (DAST.Defclass tcDef) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            let fun resolveTypeclass (name, paramName, docstring, methods) =
                    let val typarams = Set.singleton (Type.TypeParam paramName)
                    in
                        Function.Typeclass (name,
                                            paramName,
                                            docstring,
                                            map resolveMethod methods)
                    end
                and resolveMethod (DAST.MethodDecl (name, params, rt, docstring)) =
                    Function.MethodDecl (name,
                                         map mapParam params,
                                         rt,
                                         docstring)
                and mapParam (DAST.Param (name, ty)) =
                    Function.Param (Symbol.varSymbol name,
                                    ty)
            in
                let val tc = resolveTypeclass tcDef
                in
                    case Function.addTypeclass fenv tc of
                        SOME fenv' => Compiler (menv, macenv, tenv, fenv', module, code)
                      | _ => raise Fail "Duplicate typeclass definition"
                end
            end
        end
      | declareTopForm c (DAST.Definstance (name, DAST.InstanceArg (arg, typarams), docstring, methods)) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            let fun resolveMethod (DAST.MethodDef (name, params, rt, docstring, body)) =
                    Function.MethodDef (name,
                                        map mapParam params,
                                        rt,
                                        docstring)
                and mapParam (DAST.Param (name, ty)) =
                    (Function.Param (Symbol.varSymbol name,
                                     ty))
            in
                let val ins = Function.Instance (name,
                                                 Function.InstanceArg (name, typarams),
                                                 docstring,
                                                 map resolveMethod methods)
                in
                    case Function.addInstance fenv ins of
                        SOME fenv' => Compiler (menv, macenv, tenv, fenv', module, code)
                      | _ => raise Fail "Bad instance definition"
                end
            end
        end
      | declareTopForm c (DAST.Deftype (name, params, docstring, ty)) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            case (Type.addTypeAlias tenv (name, params, ty)) of
                SOME tenv' => Compiler (menv, macenv, tenv', fenv, module, code)
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (DAST.Defdisjunction (name, params, docstring, variants)) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            case (Type.addDisjunction tenv (name, params, variants)) of
                SOME tenv' => Compiler (menv, macenv, tenv', fenv, module, code)
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (DAST.Deftemplate _) =
        raise Fail "declare deftemplate not implemented"
      | declareTopForm c (DAST.DefineSymbolMacro mac) =
        let val (Compiler (menv, macenv, tenv, fenv, moduleName, code)) = c
        in
            case Macro.addSymbolMacro macenv (Macro.SymbolMacro mac) of
                SOME macenv' => Compiler (menv, macenv', tenv, fenv, moduleName, code)
              | _ => raise Fail "Duplicate symbol macro definition"
        end
      | declareTopForm c (DAST.Defmodule (name, clauses)) =
        let val (Compiler (menv, macenv, tenv, fenv, moduleName, code)) = c
        in
            let val module = Module.resolveModule menv name clauses
            in
                case Module.envGet menv name of
                    SOME _ => raise Fail "Duplicate module definition"
                  | NONE => let val menv' = Module.addModule menv module
                            in
                                Compiler (menv', macenv, tenv, fenv, moduleName, code)
                            end
            end
        end
      | declareTopForm c (DAST.InModule moduleName) =
        (* Switch current module *)
        let val (Compiler (menv, macenv, tenv, fenv, currModuleName, code)) = c
        in
            let val newModule = case Module.envGet menv moduleName of
                                    SOME m => m
                                  | NONE => raise Fail "in-module: no module with this name"
            in
                Compiler (menv, macenv, tenv, fenv, moduleName, code)
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
                            and (Compiler (menv, macenv, tenv, fenv, currMod, code)) = c
                        in
                            Compiler (menv, macenv, tenv, fenv, currMod, code ^ "\n\n" ^ cppStr)
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
