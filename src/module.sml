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

structure Module : MODULE = struct
    open Symbol

    type nicknames = (module_name, module_name) Map.map

    datatype imports = Imports of (symbol_name, module_name) Map.map

    datatype exports = Exports of symbol_name Set.set

    datatype module = Module of module_name * nicknames * imports * exports * string option

    datatype menv = MEnv of (module_name, module) Map.map

    fun moduleName (Module (n, _, _, _, _)) = n
    fun moduleNicknames (Module (_, ns, _, _, _)) = ns
    fun moduleExports (Module (_, _, _, Exports e, _)) = e
    fun moduleImports (Module (_, _, Imports i, _, _)) = i

    val emptyEnv = MEnv (Map.empty)

    fun addModule (MEnv map) m =
        MEnv (Map.iadd map (moduleName m, m))

    fun envGet (MEnv map) n =
        Map.get map n

    fun resolveNickname (m: module) (n: module_name) =
        case Map.get (moduleNicknames m) n of
            SOME n => n
          | NONE => n

    fun sourceModule menv m s =
        (* See if the current module imports a symbol with this name. If not,
           just return the current module name. If it does, get that module, and
           run sourceModule again on that module to transtively get the original
           module. *)
        case (Map.get (moduleImports m) s) of
            SOME modName => (case (envGet menv modName) of
                                 SOME sourceMod => sourceModule menv sourceMod s
                               | NONE => raise Fail "Module not found in menv")
          | NONE => moduleName m

    fun doesModuleExport (m: module) (s: symbol_name) =
        Set.isIn (moduleExports m) s

    val defaultMenv =
        let val australExports = [
                "nil",
                "true",
                "false",
                "not",
                "and",
                "or",
                "progn",
                "let",
                "if",
                "the",
                "+",
                "-",
                "*",
                "/",
                "&+",
                "&-",
                "&*",
                "&/",
                "^+",
                "^-",
                "^*",
                "^/",
                ".+",
                ".-",
                ".*",
                "./",
                "tuple",
                "proj",
                "static-array-length",
                "allocate",
                "load",
                "store",
                "defun",
                "defgeneric",
                "deftype",
                "defdisjunction",
                "defclass",
                "definstance",
                "deftemplate",
                "define-symbol-macro",
                "defmodule",
                "in-module",
                "unit",
                "boolean",
                "u8",
                "i8",
                "u16",
                "i16",
                "u32",
                "i32",
                "f32",
                "f64",
                "static-array"
            ]
            and kernelExports = [
                "eq",
                "<",
                "<=",
                ">",
                ">="
            ]
            and cffiExports = [
                "foreign-funcall",
                "malloc",
                "free",
                "null-pointer",
                "null?"
            ]
        in
            let val australMod = Module (Ident.mkIdentEx "austral",
                                         Map.empty,
                                         Imports Map.empty,
                                         Exports (Set.fromList (map Ident.mkIdentEx australExports)),
                                         NONE)
            in
                let val australUserMod = Module (Ident.mkIdentEx "austral-user",
                                                 Map.empty,
                                                 (* We import everything austral exports *)
                                                 Imports (Map.fromList (map (fn n => (Ident.mkIdentEx n,
                                                                                      Ident.mkIdentEx "austral"))
                                                                            australExports)),
                                                 Exports Set.empty,
                                                 NONE)
                    and australKernelMod = Module (Ident.mkIdentEx "austral.kernel",
                                                   Map.empty,
                                                   Imports Map.empty,
                                                   Exports (Set.fromList (map Ident.mkIdentEx kernelExports)),
                                                   NONE)
                    and australExtCffiMod = Module (Ident.mkIdentEx "austral.ext.cffi",
                                                    Map.empty,
                                                    Imports Map.empty,
                                                    Exports (Set.fromList (map Ident.mkIdentEx cffiExports)),
                                                    NONE)
                in
                    let fun envFromModules (head::tail) =
                            addModule (envFromModules tail) head
                          | envFromModules nil =
                            emptyEnv
                    in
                        envFromModules [
                            australMod,
                            australUserMod,
                            australKernelMod,
                            australExtCffiMod
                        ]
                    end
                end
            end
        end

    (* Defmodule *)

    datatype defmodule_clause = NicknamesClause of (Symbol.symbol_name * Symbol.module_name) list
                              | UseClause of Symbol.module_name list
                              | ImportFromClause of Symbol.module_name * (Symbol.symbol_name list)
                              | ExportClause of Symbol.symbol_name list
                              | DocstringClause of string

    fun resolveModule menv name clauses =
        let val nicknames = resolveNicknames clauses menv
            and imports = resolveImports clauses menv
            and exports = resolveExports clauses
            and docstring = resolveDocstring clauses
        in
            Module (name, nicknames, imports, exports, docstring)
        end
    and resolveNicknames clauses menv =
        let fun transformClause (NicknamesClause pairs) = SOME pairs
              | transformClause _ = NONE
            and processNicknames (head::tail) m =
                let val m' = processNickname head m
                in
                    processNicknames tail m'
                end
              | processNicknames nil m =
                m
            and processNickname (symbolName, moduleName) m =
                case envGet menv moduleName of
                    SOME _ => (case Map.get m symbolName of
                                   SOME _ => raise Fail "Duplicate nickname"
                                 | _ => Map.iadd m (symbolName, moduleName))
                  | NONE => raise Fail "Bad nickname: module does not exist"
        in
            let val pairs = List.concat (List.mapPartial transformClause clauses)
            in
                processNicknames pairs Map.empty
            end
        end
    and resolveImports clauses menv =
        (* Given a module name, return a pair (moduleName, exports) *)
        let fun useToImports moduleName =
                (case envGet menv moduleName of
                     SOME m => (moduleName, Set.toList (moduleExports m))
                   | NONE => raise Fail "Not module with this name")
            (* map use clauses to a list of import clauses, import clauses to
               themselves, and drop everything else *)
            and transformClause (ImportFromClause i) = SOME [i]
              | transformClause (UseClause moduleNames) = SOME (map useToImports moduleNames)
              | transformClause _ = NONE
            (* Given the pair from an import-from clause of module name and
               symbols, return a list of (symbolName, moduleName) pairs *)
            and splitImports (module, syms) =
                map (fn s => (s, module)) syms
            (* See below *)
            and processImports (head::tail) m =
                let val m' = processImport head m
                in
                    processImports tail m'
                end
              | processImports nil m =
                m
            (* Given a pair of a symbol name and module name, and a map of of
               these pairs, either add the pair to the map or signal an error if a
               duplicate import is being attempted *)
            and processImport (symbolName, moduleName) m =
                (case Map.get m symbolName of
                     SOME moduleName' => raise Fail "Duplicate import"
                   | NONE => Map.iadd m (symbolName, moduleName))
        in
            let val imports = List.concat (List.mapPartial transformClause clauses)
            in
                let val imports': (symbol_name * module_name) list =
                        List.concat (map splitImports imports)
                in
                    Imports (processImports imports' Map.empty)
                end
            end
        end
    and resolveExports clauses =
        let fun transformClause (ExportClause symbols) = SOME symbols
              | transformClause _ = NONE
        in
            let val exports = List.concat (List.mapPartial transformClause clauses)
            in
                Exports (Set.fromList exports)
            end
        end
    and resolveDocstring clauses =
        let fun transformClause (DocstringClause s) = SOME s
              | transformClause _ = NONE
        in
            case List.mapPartial transformClause clauses of
                (x::xs::xs') => raise Fail "Only a single :documentation clause is allowed in a defmodule form"
              | [s] => SOME s
              | _ => NONE
        end
end
