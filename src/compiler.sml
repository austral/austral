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
    (* Compiler type *)

    type code = string

    datatype compiler = Compiler of Module.menv
                                    * Macro.macroenv
                                    * Type.tenv
                                    * Function.fenv
                                    * MTAST.context
                                    * Symbol.module_name
                                    * code

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Macro.emptyMacroEnv,
                                  Type.defaultTenv,
                                  Function.defaultFenv,
                                  MTAST.emptyContext,
                                  Ident.mkIdentEx "austral-user",
                                  CRuntime.prelude)

    (* Accessors *)

    fun compilerMenv (Compiler (menv, _, _, _, _, _, _)) =
        menv

    fun compilerMacEnv (Compiler (_, macenv, _, _, _, _, _)) =
        macenv

    fun compilerTenv (Compiler (_, _, tenv, _, _, _, _)) =
        tenv

    fun compilerFenv (Compiler (_, _, _, fenv, _, _, _)) =
        fenv

    fun currentModule (Compiler (menv, _, _, _, _, modName, _)) =
        Option.valOf (Module.envGet menv modName)

    fun compilerCode (Compiler (_, _, _, _, _, code)) =
        code

    (* Constructors *)

    fun compilerFromFenv c fenv =
        let val (Compiler (menv, macenv, tenv, _, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    fun compilerFromTenv c tenv =
        let val (Compiler (menv, macenv, _, fenv, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    fun compilerFromMacEnv c macenv =
        let val (Compiler (menv, _, tenv, fenv, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    fun compilerFromMenv c menv =
        let val (Compiler (_, macenv, tenv, fenv, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    fun compilerFromModName c modname =
        let val (Compiler (menv, macenv, tenv, fenv, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    fun compilerFromCode c code =
        let val (Compiler (menv, macenv, tenv, fenv, modname, _)) = c
        in
            Compiler (menv, macenv, tenv, fenv, modname, code)
        end

    (* Compilation units *)

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
        let val fenv = compilerFenv c
        in
            let val f = Function.Function (name,
                                           map (fn (DAST.Param (n, t)) => Function.Param (Symbol.varSymbol n, t))
                                               params,
                                           rt,
                                           docstring)
            in
                case (Function.addFunction fenv f) of
                    SOME fenv' => compilerFromFenv c fenv'
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (DAST.Defgeneric (name, typarams, params, rt, docstring, ast)) =
        (* Add a generic function to the compiler fenv *)
        let val fenv = compilerFenv c
        in
            let val gf = Function.GenericFunction (name,
                                                   typarams,
                                                   map (fn (DAST.Param (n, t)) => Function.Param (Symbol.varSymbol n, t))
                                                       params,
                                                   rt,
                                                   docstring)
            in
                case (Function.addGenericFunction fenv gf) of
                    SOME fenv' => compilerFromFenv c fenv'
                  | NONE => raise Fail "Repeat function"
            end
        end
      | declareTopForm c (DAST.Defclass tcDef) =
        let val fenv = compilerFenv c
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
                        SOME fenv' => compilerFromFenv c fenv'
                      | _ => raise Fail "Duplicate typeclass definition"
                end
            end
        end
      | declareTopForm c (DAST.Definstance (name, DAST.InstanceArg (arg, typarams), docstring, methods)) =
        let val fenv = compilerFenv c
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
                        SOME fenv' => compilerFromFenv c fenv'
                      | _ => raise Fail "Bad instance definition"
                end
            end
        end
      | declareTopForm c (DAST.Deftype (name, params, docstring, ty)) =
        let val tenv = compilerTenv c
        in
            case (Type.addTypeAlias tenv (name, params, ty)) of
                SOME tenv' => compilerFromTenv c tenv'
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (DAST.Defdisjunction (name, params, docstring, variants)) =
        let val tenv = compilerTenv c
        in
            case (Type.addDisjunction tenv (name, params, variants)) of
                SOME tenv' => compilerFromTenv c tenv'
              | NONE => raise Fail "Duplicate type definition"
        end
      | declareTopForm c (DAST.Deftemplate _) =
        raise Fail "declare deftemplate not implemented"
      | declareTopForm c (DAST.DefineSymbolMacro mac) =
        let val macenv = compilerMacEnv c
        in
            case Macro.addSymbolMacro macenv (Macro.SymbolMacro mac) of
                SOME macenv' => compilerFromMacEnv c macenv'
              | _ => raise Fail "Duplicate symbol macro definition"
        end
      | declareTopForm c (DAST.Defmodule (name, clauses)) =
        let val menv = compilerMenv c
        in
            let val module = Module.resolveModule menv name clauses
            in
                case Module.envGet menv name of
                    SOME _ => raise Fail "Duplicate module definition"
                  | NONE => let val menv' = Module.addModule menv module
                            in
                                compilerFromMenv c menv'
                            end
            end
        end
      | declareTopForm c (DAST.InModule moduleName) =
        (* Switch current module *)
        let val menv = compilerMenv c
        in
            let val newModule = case Module.envGet menv moduleName of
                                    SOME m => m
                                  | NONE => raise Fail ("in-module: no module with this name: " ^ (Ident.identString moduleName))
            in
                compilerFromModName c moduleName
            end
        end
      | declareTopForm c (DAST.Defcfun (name, rawname, params, arity, rt, docstring)) =
        let val fenv = compilerFenv c
        in
            let val ff = Function.ForeignFunction (name,
                                                   rawname,
                                                   map (fn (DAST.Param (n, t)) => Function.Param (Symbol.varSymbol n, t))
                                                       params,
                                                   arity,
                                                   rt,
                                                   docstring)
            in
                case (Function.addForeignFunction fenv ff) of
                    SOME fenv' => compilerFromFenv c fenv'
                  | NONE => raise Fail "Error defining defcfun"
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
        open TAST
    in
    fun compileForm c topNode =
        let val tastNode = TAST.augmentTop topNode
                                           (compilerTenv c)
                                           (compilerFenv c)
        in
            raise Fail "compiler machine is kill"
            (*let val mtastNode = MTAST
            let val hirTop = HirPass.transformTop typedNode
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
            *)
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

    fun compilePrelude c =
        compileUnits c (map ReplUnit Prelude.prelude)

    fun compileEntrypoint c name =
        let val code = compilerCode c
            and sym = Parser.parseQualifiedSymbol name
        in
            let val name = CBackend.escapeSymbol sym
            in
                let val newCode = code ^ "\n\nint main() {\n  return " ^ name ^ "();\n}\n"
                in
                    compilerFromCode c newCode
                end
            end
        end
end
