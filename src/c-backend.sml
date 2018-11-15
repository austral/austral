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

    val disjTagFieldName = "tag"

    val disjDataFieldName = "data"

    fun regName r =
        "_A_r" ^ (Int.toString r)

    fun tupleIdxName i =
        "_" ^ Int.toString i

    fun genericFuncName name id =
        "_A_generic_" ^ (Int.toString id)

    (* Transform types *)

    structure C = CAst

    val boolType = C.NamedType "bool"

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
      | transform (LIR.CompOp (oper, lhs, rhs)) ty =
        let fun mapOper Builtin.EqualTo = C.EqualTo
              | mapOper Builtin.NotEqualTo = C.NotEqualTo
              | mapOper Builtin.GreaterThan = C.GreaterThan
              | mapOper Builtin.LessThan = C.LessThan
              | mapOper Builtin.GreaterThanEq = C.GreaterThanEq
              | mapOper Builtin.LessThanEq = C.LessThanEq
        in
            C.Binop (mapOper oper, transformOperand lhs, transformOperand rhs)
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
      | transform (LIR.ArrayLength arr) ty =
        raise Fail "array length: Not implemented yet"
      | transform (LIR.ArrayPointer arr) ty =
        raise Fail "array pointer: Not implemented yet"
      | transform (LIR.Load ptr) _ =
        C.Deref (transformOperand ptr)
      | transform (LIR.Construct (ty, id, SOME value)) _ =
        (* Disjunction constructors are compiled to a structure initializer *)
        C.StructInitializer (transformType ty,
                             [(disjTagFieldName, C.IntConstant (Int.toString id)),
                              (disjDataFieldName, transformOperand value)])
      | transform (LIR.Construct (ty, id, NONE)) _ =
        C.StructInitializer (transformType ty,
                             [(disjTagFieldName, C.IntConstant (Int.toString id)),
                              (disjDataFieldName, C.IntConstant "0")])
      | transform (LIR.UnsafeExtractCase (oper, id)) ty =
        C.StructAccess (transformOperand oper, disjDataFieldName)
      | transform (LIR.ForeignFuncall (name, args)) ty =
        C.Funcall (name, map transformOperand args)
      | transform (LIR.ForeignNull ty) _ =
        C.NullConstant
      | transform (LIR.SizeOf ty) _ =
        C.SizeOf (transformType ty)
      | transform (LIR.AddressOf var) _ =
        C.AddressOf (C.Variable (escapeVariable var))
      | transform (LIR.Cast (ty, oper)) _ =
        C.Cast (transformType ty, transformOperand oper)
      | transform (LIR.ConcreteFuncall (name, args)) _ =
        C.Funcall (escapeSymbol name, map transformOperand args)
      | transform (LIR.GenericFuncall (name, id, args)) _ =
        C.Funcall (genericFuncName name id, map transformOperand args)

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

    (* Transforming instructions *)

    fun transformInst (LIR.Assignment (reg, oper, ty)) =
        let val var = regName reg
            and ty' = transformType ty
        in
            C.DeclareAssign (ty', var, transform oper ty)
        end
      | transformInst (LIR.DeclareLocal (var, ty, operand)) =
        let val var' = escapeVariable var
            and ty' = transformType ty
        in
            C.DeclareAssign (ty', var', transformOperand operand)
        end
      | transformInst (LIR.Cond { test, consequent, consequent_res, alternate, alternate_res, result, ty }) =
        let val result' = regName result
            and ty' = transformType ty
        in
            C.Sequence [
                C.Declare (ty', result'),
                C.Cond (transformOperand test,
                        C.Sequence [
                            C.Sequence (map transformInst consequent),
                            C.Assign (C.Variable result', transformOperand consequent_res)
                        ],
                        C.Sequence [
                            C.Sequence (map transformInst alternate),
                            C.Assign (C.Variable result', transformOperand alternate_res)
                       ])
            ]
        end
      | transformInst (LIR.Store { ptr, value, result, ty}) =
        let val ptr' = transformOperand ptr
            and value' = transformOperand value
            and ty' = transformType ty
        in
            C.Sequence [
                C.Assign (C.Deref ptr', value'),
                C.DeclareAssign (ty', regName result, ptr')
            ]
        end
      | transformInst (LIR.Case (oper, variants, result, ty)) =
        let val oper' = transformOperand oper
            and result' = regName result
            and ty' = transformType ty
        in
            C.Sequence [
                C.Declare (ty', result'),
                C.Switch (C.StructAccess (oper', disjTagFieldName),
                          map (fn (LIR.VariantCase (id, insts, oper, ty)) =>
                                  let val insts' = map transformInst insts
                                      and oper' = transformOperand oper
                                  in
                                      (id, C.Sequence [
                                           C.Sequence insts',
                                           C.Assign (C.Variable result', oper')
                                       ])
                                  end)
                              variants)
            ]
        end
      | transformInst (LIR.VoidForeignFuncall (name, args)) =
        (C.VoidFuncall (name, map transformOperand args))

    (* Transforming top ast *)

    fun transformTop (LIR.Defun (name, params, ty, insts, oper)) =
        C.FunctionDef (escapeSymbol name,
                       mapParams params,
                       transformType ty,
                       C.Sequence (map transformInst insts),
                       transformOperand oper)
      | transformTop (LIR.DefunMonomorph (name, params, ty, insts, oper, id)) =
        C.FunctionDef (genericFuncName name id,
                       mapParams params,
                       transformType ty,
                       C.Sequence (map transformInst insts),
                       transformOperand oper)
      | transformTop (LIR.DeftypeMonomorph (name, ty, id)) =
        raise Fail "deftype-monormoph: Not implemented yet"
      | transformTop (LIR.Deftuple (id, tys)) =
        let val name = tupleName id
        in
            C.TypeDef (name,
                       C.Struct (Util.mapidx (fn (ty, idx) =>
                                                 (transformType ty, tupleIdxName idx))
                                             tys))
        end
      | transformTop (LIR.ToplevelProgn nodes) =
        C.ToplevelProgn (map transformTop nodes)

    and mapParams params =
        map (fn (LIR.Param (var, ty)) =>
                               C.Param (escapeVariable var, transformType ty))
            params
end
