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

structure Ident : IDENT = struct
    datatype ident = Identifier of string

    val alphabet =
        let val alpha = "abcdefghijklmnopqrstuvwxyz"
            and alphaup = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            and num = "0123456789"
            and sym = "!%&$#+-*/<=>?@\\~^|"
        in
            alpha ^ alphaup ^ num ^ sym
        end

    fun isValid s =
        let val sigma = explode alphabet
        in
            List.all (fn c => Util.member c sigma) (explode s)
        end

    fun mkIdent s =
        if isValid s then
            SOME (Identifier s)
        else
            NONE

    fun mkIdentEx s =
        if isValid s then
            Identifier s
        else
            raise Fail "Not a valid identifier. This is an internal compiler bug stemming from a different between the Ident structure's definition of an identifier and the parser's definition."

    fun identString (Identifier s) = s
end
