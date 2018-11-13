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

structure CBackend :> C_BACKEND = struct
    type ty = CAst.ty

    (* Escaping *)

    fun escapeSymbol symbol =
        let val module = Symbol.symbolModuleName symbol
            and name = Symbol.symbolName symbol
        in
            "_A_" ^ (escapeIdent module) ^ "___" ^ (escapeIdent name)
        end
    and escapeIdent i =
        escapeString (Ident.identString i)
    and escapeString s =
        String.concat (map escapeChar (String.explode s))
    and escapeChar #"!" = "_ba"
      | escapeChar #"%" = "_pe"
      | escapeChar #"&" = "_am"
      | escapeChar #"$" = "_do"
      | escapeChar #"#" = "_po"
      | escapeChar #"+" = "_pl"
      | escapeChar #"-" = "_da"
      | escapeChar #"*" = "_mu"
      | escapeChar #"/" = "_di"
      | escapeChar #"<" = "_lt"
      | escapeChar #"=" = "_eq"
      | escapeChar #">" = "_gt"
      | escapeChar #"?" = "_in"
      | escapeChar #"@" = "_at"
      | escapeChar #"\\" = "_bs"
      | escapeChar #"~" = "_ti"
      | escapeChar #"^" = "_ca"
      | escapeChar #"|" = "_pi"
      | escapeChar #"'" = "_qu"
      | escapeChar #"." = "_pe"
      | escapeChar c = str c

    fun escapeVariable (Symbol.Var (sym, i)) =
        (escapeSymbol sym) ^ "_" ^ (Int.toString i)

    (* Names *)

    fun tupleName id =
        "_A_tuple_" ^ (Int.toString id)

    fun disjName name id =
        "_A_" ^ (escapeSymbol name) ^ "_" ^ (Int.toString id)

    fun regName r =
        "_A_r" ^ (Int.toString r)

    fun tupleIdxName i =
        "_" ^ Int.toString i

    (* Transform types *)

    structure C = CAst

    val boolType = C.NamedType "_A_bool"

    val sizeType = C.NamedType "size_t"

    fun transformType LIR.Bool =
        boolType
      | transformType (LIR.Integer (s, w)) =
        C.NamedType (intTypeName s w)
      | transformType (LIR.Float Type.Single) =
        C.NamedType "float"
      | transformType (LIR.Float Type.Double) =
        C.NamedType "double"
      | transformType (LIR.Tuple id) =
        C.NamedType (tupleName id)
      | transformType (LIR.Pointer t) =
        C.Pointer (transformType t)
      | transformType (LIR.StaticArray t) =
        raise Fail "Static arrays not implemented yet"
      | transformType (LIR.Disjunction (name, id)) =
        C.NamedType (disjName name id)

    and intTypeName Type.Unsigned Type.Int8 =
        "uint8_t"
      | intTypeName Type.Signed   Type.Int8 =
        "int8_t"
      | intTypeName Type.Unsigned Type.Int16 =
        "uint16_t"
      | intTypeName Type.Signed   Type.Int16 =
        "int16_t"
      | intTypeName Type.Unsigned Type.Int32 =
        "uint32_t"
      | intTypeName Type.Signed   Type.Int32 =
        "int32_t"
      | intTypeName Type.Unsigned Type.Int64 =
        "uint64_t"
      | intTypeName Type.Signed   Type.Int64 =
        "int64_t"

    (* Transform code *)

    fun unwrapInt (LIR.Integer (s, w)) =
        (s, w)
      | unwrapInt _ =
        raise Fail "Internal error: not an integer type"

    fun transformOperand (LIR.BoolConstant b) =
        C.BoolConstant b
      | transformOperand (LIR.IntConstant (i, ty)) =
        C.Cast (transformType ty, C.IntConstant i)
      | transformOperand (LIR.FloatConstant (f, ty)) =
        C.Cast (transformType ty, C.FloatConstant f)
      | transformOperand (LIR.StringConstant s) =
        C.StringConstant (CST.unescapeString s)
      | transformOperand (LIR.RegisterOp r) =
        C.Variable (regName r)
      | transformOperand (LIR.VariableOp (var, _)) =
        C.Variable (escapeVariable var)

    fun transform (LIR.ArithOp (kind, oper, lhs, rhs)) ty =
        let val lhs = transformOperand lhs
            and rhs = transformOperand rhs
        in
            let val f = case kind of
                            Arith.Modular => transformModularArith
                          | Arith.Checked => transformCheckedArith
                          | Arith.Saturation => transformSaturationArith
                          | Arith.Float => transformFloatArith
            in
                (f ty oper lhs rhs)
            end
        end
      | transform (LIR.TupleCreate operands) ty =
        (* For tuple creation, we create a struct initializer and cast it to the
           given tuple type *)
        let val args = Util.mapidx (fn (oper, idx) =>
                                       (tupleIdxName idx, transformOperand oper))
                                   operands
        in
            C.StructInitializer (transformType ty, args)
        end
      | transform (LIR.TupleProj (tup, idx)) _ =
        C.StructAccess (transformOperand tup, tupleIdxName idx)

      | transform _ _ =
        raise Fail "Not implemented yet"

    and transformModularArith ty oper lhs rhs =
        (* Modular arithmetic is implemented directly, except for division,
           where we call a function to check if the divisor is zero and fail at
           runtime *)
        case oper of
            Arith.Div => let val (s, w) = unwrapInt ty
                         in
                             let val name = "austral_div_" ^ (intTypeName s w)
                             in
                                 C.Funcall (name, [lhs, rhs])
                             end
                         end
          | _ => C.Binop (mapOper oper, lhs, rhs)

    and transformCheckedArith ty oper lhs rhs =
        raise Fail "checked arith not implemented yet"

    and transformSaturationArith ty oper lhs rhs =
        raise Fail "saturation arith not implemented yet"

    and transformFloatArith ty oper lhs rhs =
        (* Floating point arithmetic is implemented directly. *)
        C.Binop (mapOper oper, lhs, rhs)

    and mapOper Arith.Add =
        C.Add
      | mapOper Arith.Sub =
        C.Sub
      | mapOper Arith.Mul =
        C.Mul
      | mapOper Arith.Div =
        C.Div
end
