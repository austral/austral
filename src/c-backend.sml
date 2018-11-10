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

    local
        open Symbol
    in
        fun escapeSymbol symbol =
            let val module = symbolModuleName symbol
                and name = symbolName symbol
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
    end

    (* Tuples *)

    type tuple_types = (ty list, ty) Map.map

    val emptyTupleTypes =
        Map.empty

    fun getTuple tt tys =
        Map.get tt tys

    val count = ref 0
    fun freshId () =
        (count := !count + 1;
         !count)

    fun addTuple tt tys =
        case getTuple tt tys of
            (SOME ty) => (ty, tt)
          | NONE => let val id = freshId ()
                    in
                        let val ty = CAst.NamedType ("_A_tuple_" ^ (Int.toString id))
                        in
                            (ty, Map.iadd tt (tys, ty))
                        end
                    end

    (* Transform types *)

    val boolType = CAst.NamedType "_A_bool"

    val sizeType = CAst.NamedType "size_t"

    fun disjName name id =
        "_A_" ^ (escapeSymbol name) ^ "_" ^ (Int.toString id)

    structure C = CAst

    local
        open CAst
    in
        fun transformType tt MIR.Unit =
            (boolType, tt)
          | transformType tt MIR.Bool =
            (boolType, tt)
          | transformType tt (MIR.Integer (s, w)) =
            (NamedType (transformIntType s w), tt)
          | transformType tt (MIR.Float Type.Single) =
            (NamedType "float", tt)
          | transformType tt (MIR.Float Type.Double) =
            (NamedType "double", tt)
          | transformType tt (MIR.Tuple tys) =
            let val (tys', tt) = Util.foldThread (fn (ty, tt) =>
                                                     transformType tt ty)
                                                 tys
                                                 tt
            in
                addTuple tt tys'
            end
          | transformType tt (MIR.Pointer t) =
            let val (t', tt) = transformType tt t
            in
                (Pointer t', tt)
            end
          | transformType tt (MIR.StaticArray t) =
            let val (t', tt) = transformType tt t
            in
                addTuple tt [sizeType, t']
            end
          | transformType tt (MIR.Disjunction (name, id)) =
            (CAst.NamedType (disjName name id), tt)

        and transformIntType Type.Unsigned Type.Int8 =
            "uint8_t"
          | transformIntType Type.Signed   Type.Int8 =
            "int8_t"
          | transformIntType Type.Unsigned Type.Int16 =
            "uint16_t"
          | transformIntType Type.Signed   Type.Int16 =
            "int16_t"
          | transformIntType Type.Unsigned Type.Int32 =
            "uint32_t"
          | transformIntType Type.Signed   Type.Int32 =
            "int32_t"
          | transformIntType Type.Unsigned Type.Int64 =
            "uint64_t"
          | transformIntType Type.Signed   Type.Int64 =
            "int64_t"
    end

    (* Transform code *)

    fun regName r =
        "_A_r" ^ (Int.toString r)

    fun transformOperand _ MIR.UnitConstant =
        C.BoolConstant false
      | transformOperand _ (MIR.BoolConstant b) =
        C.BoolConstant b
      | transformOperand tt (MIR.IntConstant (i, ty)) =
        let val (ty', _) = transformType tt ty
        in
            C.Cast (ty', C.IntConstant i)
        end
      | transformOperand tt (MIR.FloatConstant (f, ty)) =
        let val (ty', _) = transformType tt ty
        in
            C.Cast (ty', C.FloatConstant f)
        end
      | transformOperand _ (MIR.StringConstant s) =
        C.StringConstant (CST.unescapeString s)
      | transformOperand _(MIR.RegisterOp r) =
        C.Variable (regName r)
      | transformOperand _ (MIR.VariableOp (var, _)) =
        C.Variable (escapeVariable var)

    fun transform tt (MIR.ArithOp (kind, oper, lhs, rhs)) =
        let val lhs = transformOperand tt lhs
            and rhs = transformOperand tt rhs
        in
            raise Fail "Not implemented yet"
        end
      | transform _ _ =
        raise Fail "Not implemented yet"
end
