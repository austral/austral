(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Compiler : COMPILER = struct
    (* Compiler type *)

    type code = string

    datatype compiler = Compiler of Module.menv
                                    * Macro.macroenv
                                    * Type.tenv
                                    * Function.fenv
                                    * MTAST.context
                                    * FDefs.fdefenv
                                    * LirPass.tuple_types
                                    * Symbol.module_name
                                    * code

    val emptyCompiler = Compiler (Module.defaultMenv,
                                  Macro.emptyMacroEnv,
                                  Type.defaultTenv,
                                  Function.defaultFenv,
                                  MTAST.emptyContext,
                                  FDefs.emptyEnv,
                                  LirPass.emptyTupleTypes,
                                  Ident.mkIdentEx "austral-user",
                                  CRuntime.prelude)

    (* Accessors *)

    fun compilerMenv (Compiler (menv, _, _, _, _, _, _, _, _)) =
        menv

    fun compilerMacEnv (Compiler (_, macenv, _, _, _, _, _, _, _)) =
        macenv

    fun compilerTenv (Compiler (_, _, tenv, _, _, _, _, _, _)) =
        tenv

    fun compilerFenv (Compiler (_, _, _, fenv, _, _, _, _, _)) =
        fenv

    fun compilerMonoCtx (Compiler (_, _, _, _, ctx, _, _, _, _)) =
        ctx

    fun compilerFdefs (Compiler (_, _, _, _, _, fdefs, _, _, _)) =
        fdefs

    fun compilerTupleTypes (Compiler (_, _, _, _, _, _, tts, _, _)) =
        tts

    fun currentModule (Compiler (menv, _, _, _, _, _, _, modName, _)) =
        Option.valOf (Module.envGet menv modName)

    fun compilerCode (Compiler (_, _, _, _, _, _, _, _, code)) =
        code

    (* Constructors *)

    fun compilerFromFenv c fenv =
        let val (Compiler (menv, macenv, tenv, _, mtast, fdefs, tts, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromTenv c tenv =
        let val (Compiler (menv, macenv, _, fenv, mtast, fdefs, tts, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromMacEnv c macenv =
        let val (Compiler (menv, _, tenv, fenv, mtast, fdefs, tts, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromMenv c menv =
        let val (Compiler (_, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromMonoCtx c mtast =
        let val (Compiler (menv, macenv, tenv, fenv, _, fdefs, tts, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun addFundef c name params body =
        let val (Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)) = c
        in
            let val fdefs = FDefs.addDefinition fdefs name (params, body)
            in
                Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
            end
        end

    fun compilerFromTTS c tts =
        let val (Compiler (menv, macenv, tenv, fenv, mtast, fdefs, _, modname, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromModName c modname =
        let val (Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, _, code)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    fun compilerFromCode c code =
        let val (Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, _)) = c
        in
            Compiler (menv, macenv, tenv, fenv, mtast, fdefs, tts, modname, code)
        end

    (* Compilation units *)

    type pathname = string

    datatype compilation_unit = FileUnit of pathname
                              | ReplUnit of string

    fun unitForms (FileUnit path) = Parser.parseFile path
      | unitForms (ReplUnit string) = [Parser.parseString string]

    (* Declaration pass *)

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
            compilerFromTenv c (Type.addDeclaration tenv (name, params, Type.AliasDecl ty))
        end
      | declareTopForm c (DAST.Defdatatype (name, params, _, _)) =
        let val tenv = compilerTenv c
        in
            compilerFromTenv c (Type.addDeclaration tenv (name, params, Type.DisjunctionDecl))
        end
      | declareTopForm c (DAST.Defrecord (name, params, _, _)) =
        let val tenv = compilerTenv c
        in
            compilerFromTenv c (Type.addDeclaration tenv (name, params, Type.RecordDecl))
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

    (* Type definition extraction pass *)

    fun defineType c (DAST.Deftype (name, typarams, _, ty)) =
        let val tenv = compilerTenv c
        in
            compilerFromTenv c (Type.addDefinition tenv (name, typarams, Type.AliasDef ty))
        end
      | defineType c (DAST.Defdatatype (name, typarams, _, variants)) =
        let val tenv = compilerTenv c
        in
            compilerFromTenv c (Type.addDefinition tenv (name, typarams, Type.DisjunctionDef variants))
        end
      | defineType c _ =
        c

    fun defineTypePass c nodes =
        Util.foldThread (fn (node, c) =>
                            (node, defineType c node))
                        nodes
                        c

    (* Augmentation pass *)

    fun augmentForm c node =
        (TAST.augmentTop node
                         (compilerTenv c)
                         (compilerFenv c),
         c)

    fun augmentationPass c forms =
        Util.foldThread (fn (form, c) =>
                            augmentForm c form)
                        forms
                        c

    (* Definition extraction pass *)

    fun extractDefinition c (TAST.Defgeneric (name, typarams, params, ty, _, ast)) =
        addFundef c name params ast
      | extractDefinition c _ =
        c

    fun extractionPass c forms =
        let val (forms, c) = Util.foldThread (fn (form, c) =>
                                                 (form, extractDefinition c form))
                                             forms
                                             c
        in
            c
        end

    fun compileForm c node =
        let val (mtastNode, ctx) = MTAST.monomorphizeTop (compilerTenv c)
                                                         (compilerFenv c)
                                                         (compilerFdefs c)
                                                         (compilerMonoCtx c)
                                                         node
        in
            let val c = compilerFromMonoCtx c ctx
            in
                let val hirNode = HirPass.transformTop (compilerTenv c) mtastNode
                in
                    let val mirNode = MirPass.transformTop hirNode
                    in
                        let val (lirNode, tts) = LirPass.transformTop (compilerTupleTypes c)
                                                                      mirNode
                        in
                            let val c = compilerFromTTS c tts
                            in
                                let val cNode = CBackend.transformTop lirNode
                                in
                                    let val newCode = CRenderer.renderTop cNode
                                    in
                                        let val code = (compilerCode c) ^ "\n\n" ^ newCode
                                        in
                                            compilerFromCode c code
                                        end
                                    end
                                end
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
        let val (forms, c) = (declarationPass c (unitForms u))
        in
            let val (forms, c) = defineTypePass c forms
            in
                let val (forms, c) = augmentationPass c forms
                in
                    let val c = extractionPass c forms
                    in
                        compilationPass c forms
                    end
                end
            end
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
