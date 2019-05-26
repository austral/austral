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

signature ESCAPE = sig
    (* A string where escape characters have been processed into the characters they represent *)
    type escaped_string

    (* Given a string with escape characters, process the escape characters and return a escaped string *)
    val escapeString : string -> escaped_string

    (* Return a escaped string as a regular string *)
    val escapedToString : escaped_string -> string

    (* Replace unrepresentable characters in a escaped string (e.g. newlines) with escape characters *)
    val unescapeString : escaped_string -> string
end
