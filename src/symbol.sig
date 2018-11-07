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

signature SYMBOL = sig
    type module_name
    type symbol_name

    type symbol

    val mkSymbol : module_name * symbol_name -> symbol
    val symbolModuleName : symbol -> module_name
    val symbolName : symbol -> symbol_name
    val toString : symbol -> string

    datatype variable = Var of symbol * int
                      | Genvar of int

    val varSymbol : variable -> symbol
    val varToString : variable -> string

    (* Symbol utilities *)

    val au : string -> symbol
    val auKer : string -> symbol
    val auCffi : string -> symbol
end
