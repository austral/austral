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

structure Prelude :> PRELUDE = struct
    val prelude = [
        "(in-module :austral)",
        "(defgeneric austral.ext.cffi:malloc (tau) ((size u64)) (austral.ext.cffi:foreign-pointer tau)\
        \  (austral.ext.cffi:foreign-funcall \"malloc\" (austral.ext.cffi:foreign-pointer tau) size))",
        "(defgeneric austral.ext.cffi:free (tau) ((pointer (austral.ext.cffi:foreign-pointer tau))) unit\
        \  (austral.ext.cffi:foreign-funcall \"free\" unit pointer))",
        "(defgeneric austral.ext.cffi:null? (tau) ((pointer (austral.ext.cffi:foreign-pointer tau))) boolean\
        \  (austral.kernel:eq pointer (austral.ext.cffi:null-pointer tau)))",
        "(in-module :austral-user)"
    ]
end
