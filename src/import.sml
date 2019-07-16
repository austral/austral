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

signature IMPORT = sig
    type import
    type imports

    val mkImport : { name: Name.ident, trueName: Name.ident, moduleName: Name.module_name } -> import

    val importName : import -> Name.ident
    val importTrueName : import -> Name.ident
    val importModuleName : import -> Name.module_name

    (* Given a list of imports, return an `imports` object. Validates that there
       are no colliding imports. *)
    val fromList : import list -> imports
    val getImport : imports -> Name.ident -> import option
end

structure Import :> IMPORT = struct
    type name = Name.ident
    type module_name = Name.module_name

    datatype import = Import of { name: name, trueName: name, moduleName: module_name }

    type imports = import list

    val mkImport = Import

    (* Accessors *)

    fun importName (Import i) = #name i

    fun importTrueName (Import i) = #trueName i

    fun importModuleName (Import i) = #moduleName i

    (* Functions for working with import lists *)

    fun fromList imports =
        let val set = Set.fromList (map importName imports)
        in
            if Set.size set <> List.length imports then
                Error.semantic "Colliding import"
            else
                imports
        end

    fun getImport imports name =
        List.find (fn import =>
                      importName import = name)
                  imports
end
