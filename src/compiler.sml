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
                (topNode, declareTopForm compiler topNode)
            end
        end
    and declareTopForm c (AST.Defun (name, params, rt, docstring, ast)) =
        (* Add a concrete function to the compiler fenv *)
        let val (Compiler (menv, macenv, tenv, fenv, currModuleName, code)) = c
        in
            let val f = Function.Function (name,
                                           map (fn (AST.Param (n, t)) => Function.Param (Symbol.varSymbol n,
                                                                                         Type.resolve tenv Set.empty t))
                                               params,
                                           Type.resolve tenv Set.empty rt,
                                           docstring)
            in
                case (Function.addFunction fenv f) of
                    SOME fenv' => Compiler (menv, macenv, tenv, fenv', currModuleName, code)
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (AST.Defclass tcDef) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            let fun resolveTypeclass (name, paramName, docstring, methods) =
                    let val typarams = Set.singleton (Type.TypeParam paramName)
                    in
                        Function.Typeclass (name,
                                            paramName,
                                            docstring,
                                            map (resolveMethod typarams) methods)
                    end
                and resolveMethod typarams (AST.MethodDecl (name, params, rt, docstring)) =
                    Function.MethodDecl (name,
                                         map (mapParam typarams) params,
                                         Type.resolve tenv typarams rt,
                                         docstring)
                and mapParam typarams (AST.Param (name, typespec)) =
                    (Function.Param (Symbol.varSymbol name, Type.resolve tenv typarams typespec))
            in
                let val tc = resolveTypeclass tcDef
                in
                    case Function.addTypeclass fenv tc of
                        SOME fenv' => Compiler (menv, macenv, tenv, fenv', module, code)
                      | _ => raise Fail "Duplicate typeclass definition"
                end
            end
        end
      | declareTopForm c (AST.Definstance (name, AST.InstanceArg (arg, typarams), docstring, methods)) =
        let val (Compiler (menv, macenv, tenv, fenv, module, code)) = c
            and typarams = Set.fromList (map (fn n => Type.TypeParam n) (Set.toList typarams))
        in
            let fun resolveMethod (AST.MethodDef (name, params, rt, docstring, body)) =
                    Function.MethodDecl (name,
                                         map (mapParam typarams) params,
                                         Type.resolve tenv typarams rt,
                                         docstring)
                and mapParam typarams (AST.Param (name, typespec)) =
                    (Function.Param (Symbol.varSymbol name,
                                     Type.resolve tenv typarams typespec))
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
      | declareTopForm c (AST.Deftype (name, params, docstring, def)) =
        let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
            (* FIXME: this is fucked *)
            and unorderedParams' = Set.fromList (map (fn s => Type.TypeParam s) params)
            and (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            case (Type.addTypeAlias tenv (name, params', Type.resolve tenv unorderedParams' def)) of
                SOME tenv' => Compiler (menv, macenv, tenv', fenv, module, code)
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (AST.Defdisjunction (name, params, docstring, variants)) =
        let val params' = OrderedSet.fromList (map (fn s => Type.TypeParam s) params)
            (* FIXME: this is fucked *)
            and unorderedParams' = Set.fromList (map (fn s => Type.TypeParam s) params)
            and (Compiler (menv, macenv, tenv, fenv, module, code)) = c
        in
            let fun mapVariant (AST.Variant (name, SOME tys)) =
                    Type.Variant (name, SOME (Type.resolve tenv unorderedParams' tys))
                  | mapVariant (AST.Variant (name, NONE)) =
                    Type.Variant (name, NONE)
            in
                case (Type.addDisjunction tenv (name, params', map mapVariant variants)) of
                    SOME tenv' => Compiler (menv, macenv, tenv', fenv, module, code)
                  | NONE => raise Fail "Duplicate type definition"
            end
        end
      | declareTopForm c (AST.Deftemplate _) =
        raise Fail "declare deftemplate not implemented"
      | declareTopForm c (AST.DefineSymbolMacro mac) =
        let val (Compiler (menv, macenv, tenv, fenv, moduleName, code)) = c
        in
            case Macro.addSymbolMacro macenv (Macro.SymbolMacro mac) of
                SOME macenv' => Compiler (menv, macenv', tenv, fenv, moduleName, code)
              | _ => raise Fail "Duplicate symbol macro definition"
        end
      | declareTopForm c (AST.Defmodule (name, clauses)) =
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
      | declareTopForm c (AST.InModule moduleName) =
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
