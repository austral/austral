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

structure Prelude :> PRELUDE = struct
    val prelude = [
        (*"(in-module :austral.ext.cffi)",
        "(defcfun (malloc* \"malloc\") ((size austral:u64)) (foreign-pointer austral:u8))",
        "(austral:defgeneric malloc (tau) ((size austral:u64)) (foreign-pointer tau)\
        \  (cast (foreign-pointer tau) (malloc* size)))",
        "(defcfun (free* \"free\") ((pointer (foreign-pointer austral:u8))) austral:unit)",
        "(austral:defgeneric free (tau) ((pointer (foreign-pointer tau))) austral:unit\
        \  (free* (cast (foreign-pointer austral:u8) pointer)))",
        "(austral:defgeneric null? (tau) ((pointer (foreign-pointer tau))) austral:boolean\
        \  (austral.kernel:eq pointer (null-pointer tau)))",
        "(austral:in-module :austral-user)"*)
    ]
end
