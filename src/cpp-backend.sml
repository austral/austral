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

    fun transformType MIR.Bool =
        NamedType "bool"
      | transformType MIR.UInt8 =
        NamedType "uint8_t"
      | transformType MIR.SInt8 =
        NamedType "int8_t"
      | transformType MIR.UInt16 =
        NamedType "uint16_t"
      | transformType MIR.SInt16 =
        NamedType "int16_t"
      | transformType MIR.UInt32 =
        NamedType "uint32_t"
      | transformType MIR.SInt32 =
        NamedType "int32_t"
      | transformType MIR.UInt64 =
        NamedType "uint64_t"
      | transformType MIR.SInt64 =
        NamedType "int64_t"
      | transformType MIR.SingleFloat =
        NamedType "float"
      | transformType MIR.DoubleFloat =
        NamedType "double"
      | transformType (MIR.NamedType name) =
        NamedType name
      | transformType (MIR.Pointer ty) =
        Pointer (transformType ty)
      | transformType (MIR.Tuple tys) =
        TypeCons ("std::tuple", (map transformType tys))
      | transformType (MIR.TypeCons (name, tys)) =
        TypeCons (name, map transformType tys)
      | transformType (MIR.TypeVariable name) =
        NamedType name

    fun transformExp (MIR.BoolConstant b) =
        BoolConstant b
      | transformExp (MIR.IntConstant i) =
        IntConstant i
      | transformExp (MIR.FloatConstant f) =
        FloatConstant f
      | transformExp (MIR.StringConstant s) =
        StringConstant (CST.escapedToString s)
      | transformExp MIR.NullConstant =
        NullConstant
      | transformExp (MIR.Variable n) =
        Variable n
      | transformExp (MIR.Cast (ty, exp)) =
        Cast (transformType ty, transformExp exp)
      | transformExp (MIR.Load ptr) =
        Deref (transformExp ptr)
      | transformExp (MIR.AddressOf exp) =
        AddressOf (transformExp exp)
      | transformExp (MIR.SizeOf ty) =
        SizeOf (transformType ty)
      | transformExp (MIR.TupleCreate exps) =
        Funcall ("std::make_tuple", [], map transformExp exps)
      | transformExp (MIR.TupleProj (tup, idx)) =
        Funcall ("std::get",
                 [NamedType (Int.toString idx)],
                 [transformExp tup])
      | transformExp (MIR.Funcall (name, tyargs, args)) =
        Funcall (name,
                 map transformType tyargs,
                 map transformExp args)

    fun transformBlock (MIR.Progn nodes) =
        Sequence (map transformBlock nodes)
      | transformBlock (MIR.Declare (name, ty)) =
        Declare (transformType ty, name)
      | transformBlock (MIR.Assign (dest, source)) =
        Assign (transformExp dest, transformExp source)
      | transformBlock (MIR.Store (ptr, exp)) =
        Assign (Deref (transformExp ptr),
                transformExp exp)
      | transformBlock (MIR.Cond (test, cons, alt)) =
        Cond (transformExp test,
              transformBlock cons,
              transformBlock alt)

    fun transformTop (MIR.Defun (name, typarams, params, rt, body, retval)) =
        FunctionDef (name,
                     map (fn (MIR.TypeParam n) => TypeParam n) typarams,
                     map (fn (MIR.Param (n, t)) => Param (n, transformType t)) params,
                     transformType rt,
                     transformBlock body,
                     transformExp retval)
      | transformTop (MIR.ToplevelProgn nodes) =
        ToplevelProgn (map transformTop nodes)
end
