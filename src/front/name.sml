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

structure Name : NAME = struct
    (* Alphabet definition utilities *)

    val alpha = "abcdefghijklmnopqrstuvwxyz"
    val alphaup = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val num = "0123456789"

    (* Module names *)

    datatype module_name = ModuleName of string

    val moduleNameAlphabet =
        let val sym = "-."
        in
            alpha ^ alphaup ^ num ^ sym
        end

    fun isValidModuleName s =
        let val sigma = explode moduleNameAlphabet
        in
            List.all (fn c => Util.member c sigma) (explode s)
        end

    fun mkModuleName s =
        if isValidModuleName s then
            SOME (ModuleName s)
        else
            NONE

    fun mkModuleNameEx s =
        if isValidModuleName s then
            ModuleName s
        else
            raise Fail "Not a valid identifier."

    fun moduleNameString (ModuleName s) = s

    (* Identifiers *)

    datatype ident = Identifier of string

    val identAlphabet =
        let val sym = "$#?@~^'."
        in
            alpha ^ alphaup ^ num ^ sym
        end

    fun isValidIdent s =
        let val sigma = explode identAlphabet
        in
            List.all (fn c => Util.member c sigma) (explode s)
        end

    fun mkIdent s =
        if isValidIdent s then
            SOME (Identifier s)
        else
            NONE

    fun mkIdentEx s =
        if isValidIdent s then
            Identifier s
        else
            raise Fail "Not a valid identifier."

    fun identString (Identifier s) = s
end
