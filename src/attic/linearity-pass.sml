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

structure LinearityPass :> LINEARITY_PASS = struct
    type name = Symbol.symbol

    type frequency_table = (name, int) Map.map

    val emptyTable = Map.empty

    fun addAppearance table name =
        case Map.get table name of
            SOME appearances => Map.iadd table (name, appearances + 1)
          | NONE => Map.iadd table (name, 0)
end
