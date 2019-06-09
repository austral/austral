(*
    Copyright 2018–2019 Fernando Borretti <fernando@borretti.me>

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

structure Module :> MODULE = struct
    (* Types *)

    type name = Name.ident
    type module_name = Name.module_name

    type type_visibility = Syntax.type_visibility

    type func_visibility = Syntax.func_visibility

    type docstring = Syntax.docstring

    type ty = Type.ty

    type imports = (Name.ident, module_name) Map.map

    datatype module = Module of module_name * docstring * imports * (Name.ident, declaration) Map.map
         and declaration = RecordDefinition of docstring * type_visibility * name * slot_definition list
                         | UnionDefinition of docstring * type_visibility * name * case_definition list
                         | FunctionDefinition of docstring * func_visibility * name * param list * ty

         and slot_definition = SlotDefinition of name * ty * docstring

         and case_definition = CaseDefinition of name * ty option * docstring

         and param = Param of name * ty * docstring

    (* Module functions *)

    fun moduleName (Module (n, _, _, _)) =
        n

    fun getDeclaration (Module (_, _, _, ds)) name =
        Map.get ds name

    (* Module Environment

       The module environment is the collection of modules the compiler knows
       about. It's a map of module names to module instances.

    *)

    type menv = (module_name, module) Map.map

    val getModule = Map.get

    (* Module Resolution

       The following functions resolve Syntax.module objects into module
       objects.

       First, we resolve imports. Import validation is fairly straightforward:
       we have a list of import statements, each of which refers to a single
       module and imports a set of names from that module, and have to turn it
       into a map of identifiers to the name of the module that identifier comes
       from.

       In the process, we need to validate a few things:

       1. Check that imports refer to modules that exist in the module
       environment.

       2. Check that imported names refer to declarations that exist in the
       referenced module.

       3. Check that those declarations are public (in the case of functions) or
       either public or opaque (in the case of types). Otherwise, we can't
       import them.

       4. Ensure that imports don't collide with each other: if we have `from A
       import a` and `from B import a`, this is clearly an error.

    *)

    fun validateImport (Syntax.Import (moduleName, names)) menv =
        case getModule menv moduleName of
            (SOME module) => (map (validateImportedName module) names;
                              module)
          | NONE => Error.semantic ("No module with this name: " ^ (Name.moduleNameString moduleName))

    and validateImportedName module name =
        let fun getName (Syntax.ImportedName name) =
                name
              | getName (Syntax.ImportedNameAs { original, rename }) =
                original
        in
            let val name = getName name
            in
                let val decl = validateDeclarationExists module name
                in
                    if validateDeclarationVisibility decl then
                        ()
                    else
                        Error.semantic ("Attempted to import a private name: '"
                                        ^
                                        (Name.identString name)
                                        ^
                                        "' in the module '"
                                        ^
                                        (Name.moduleNameString (moduleName module))
                                        ^
                                        "' is private")
                end
            end
        end

    and validateDeclarationExists module name =
        case getDeclaration module name of
            (SOME decl) => decl
          | NONE => Error.semantic ("Imported name '"
                                    ^
                                    (Name.identString name)
                                    ^
                                    "' does not exist in module '"
                                    ^
                                    (Name.moduleNameString (moduleName module))
                                    ^
                                    "'")

    and validateDeclarationVisibility (RecordDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (UnionDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (FunctionDefinition (_, vis, _, _, _, _)) =
        validFuncVis vis

    and validTypeVis Syntax.PublicType =
        true
      | validTypeVis Syntax.OpaqueType =
        true
      | validTypeVis Syntax.PrivateType =
        false

    and validFuncVis Syntax.PublicFunction =
        true
      | validFuncVis Syntax.PrivateFunction =
        false

    (* Here we implement the remainder of module resolution. This is mostly
       mapping syntax declarations to module declarations. The validation
       perform at this stage is basically ensuring declaration names don't
       collide (with each other, or with imports). Additionally, we resolve type
       specifiers to type objects.

    *)

    fun resolve (Syntax.Module (docstring, name, imports, declarations)) =
        Module (name, docstring, resolveImports imports, resolveDeclarations declarations)

    and resolveImports importList =
        Error.notImplemented ()

    and resolveDeclarations declarationList =
        Error.notImplemented ()

    and resolveDeclaration (Syntax.RecordDefinition (ds, tv, name, slots)) =
        let fun resolveSlot (Syntax.SlotDefinition (n, ts, ds)) =
                SlotDefinition (n, resolveType ts, ds)
        in
            RecordDefinition (ds, tv, name, map resolveSlot slots)
        end
      | resolveDeclaration (Syntax.UnionDefinition (ds, tv, name, cases)) =
        let fun resolveCase (Syntax.CaseDefinition (n, ts, ds)) =
                CaseDefinition (n, Option.map resolveType ts, ds)
        in
            UnionDefinition (ds, tv, name, map resolveCase cases)
        end
      | resolveDeclaration (Syntax.FunctionDefinition (ds, fv, name, params, rt, expr)) =
        let fun resolveParam (Syntax.Param (n, ts, ds)) =
                Param (n, resolveType ts, ds)
        in
            FunctionDefinition (ds, fv, name, map resolveParam params, resolveType rt)
        end

    and resolveType (Syntax.NamedType name) =
        Error.notImplemented ()
      | resolveType (Syntax.Address ts) =
        Type.Address (resolveType ts)
      | resolveType (Syntax.Pointer ts) =
        Type.Pointer (resolveType ts)
      | resolveType (Syntax.TupleType tss) =
        Type.TupleType (map resolveType tss)
end
