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

structure CST :> CST = struct
  datatype escaped_string = EscapedString of string

  datatype cst = IntConstant of int
               | StringConstant of escaped_string
               | QualifiedSymbol of Symbol.symbol
               | UnqualifiedSymbol of Symbol.symbol_name
               | Keyword of Symbol.symbol_name
               | List of cst list

  fun escapeString s =
    EscapedString s (* TODO: actually implement this *)

  fun escapedToString (EscapedString s) = s
end
