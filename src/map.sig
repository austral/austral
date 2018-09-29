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

signature MAP = sig
    type (''k, 'v) map

    val empty : (''k, 'v) map
    val get : (''k, 'v) map -> ''k -> 'v option
    val add : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map option
    val iadd : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map
    val iaddList : (''k, 'v) map -> (''k * 'v) list -> (''k, 'v) map
    val size : (''k, 'v) map -> int

    val fromList : (''k * 'v) list -> (''k, 'v) map
end
