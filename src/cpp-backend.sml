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

structure CppBackend :> CPP_BACKEND = struct
    open CppAst

    fun transformType MIR.Bool = NamedType "bool"
      | transformType MIR.UInt8 = NamedType "uint8_t"
      | transformType MIR.SInt8 = NamedType "int8_t"
      | transformType MIR.UInt16 = NamedType "uint16_t"
      | transformType MIR.SInt16 = NamedType "int16_t"
      | transformType MIR.UInt32 = NamedType "uint32_t"
      | transformType MIR.SInt32 = NamedType "int32_t"
      | transformType MIR.UInt64 = NamedType "uint64_t"
      | transformType MIR.SInt64 = NamedType "int64_t"
      | transformType MIR.SingleFloat = NamedType "float"
      | transformType MIR.DoubleFloat = NamedType "double"
      | transformType (MIR.NamedType name) = NamedType name
end
