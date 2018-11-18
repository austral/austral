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

structure HirPass :> HIR_PASS = struct
    open HIR
    structure MT = MonoType

    val count = ref 0
    fun freshVar () =
        let
        in
            count := !count + 1;
            let val newid = !count
            in
                let val name = Symbol.mkSymbol (Ident.mkIdentEx "#",
                                                Ident.mkIdentEx ("g" ^ (Int.toString newid)))
                in
                    Symbol.Var (name, newid)
                end
            end
        end

    fun transformType MT.Unit =
        Unit
      | transformType MT.Bool =
        Bool
      | transformType (MT.Integer (s, w)) =
        Integer (s, w)
      | transformType (MT.Float f) =
        Float f
      | transformType (MT.Tuple tys) =
        Tuple (map transformType tys)
      | transformType (MT.Pointer ty) =
        Pointer (transformType ty)
      | transformType (MT.Address ty) =
        Pointer (transformType ty)
      | transformType (MT.PositiveAddress ty) =
        Pointer (transformType ty)
      | transformType (MT.StaticArray ty) =
        StaticArray (transformType ty)
      | transformType (MT.Disjunction (name, id, _)) =
        Disjunction (name, id)

    fun caseNameIdx ty name =
        let fun nameVariantsIdx variants name =
                Option.valOf (Util.position name (map (fn (MonoType.Variant (name, _)) => name) variants))
        in
            case ty of
                (MonoType.Disjunction (_, _, variants)) => nameVariantsIdx variants name
              | _ => raise Fail "Internal error: not a disjunction"
        end

    structure M = MTAST

    fun transform M.UnitConstant =
        UnitConstant
      | transform (M.BoolConstant b) =
        BoolConstant b
      | transform (M.IntConstant (i, ty)) =
        IntConstant (i, transformType ty)
      | transform (M.FloatConstant (f, ty)) =
        FloatConstant (f, transformType ty)
      | transform (M.StringConstant s) =
        StringConstant s
      | transform (M.Variable (var, ty)) =
        Variable (var, transformType ty)
      | transform (M.Let (var, value, body)) =
        Let (var, transform value, transform body)
      | transform (M.Bind (vars, tup, body)) =
        (* Since we drop linearity in HIR, we can turn bind expressions into a
           Let that simply projects each tuple element. *)
        let val tupvar = freshVar ()
        in
            Let (tupvar,
                 transform tup,
                 transformBind (transformType (M.typeOf tup)) vars tupvar body)
        end
      | transform (M.Cond (t, c, a)) =
        Cond (transform t, transform c, transform a)
      | transform (M.ArithOp (kind, oper, lhs, rhs)) =
        ArithOp (kind, oper, transform lhs, transform rhs)
      | transform (M.CompOp (oper, lhs, rhs)) =
        CompOp (oper, transform lhs, transform rhs)
      | transform (M.TupleCreate elems) =
        TupleCreate (map transform elems)
      | transform (M.TupleProj (tup, idx)) =
        TupleProj (transform tup, idx)
      | transform (M.ArrayLength arr) =
        ArrayLength (transform arr)
      | transform (M.ArrayPointer arr) =
        ArrayPointer (transform arr)
      | transform (M.Malloc (ty, len)) =
        let val ty' = transformType ty
            and len' = transform len
        in
            Cast (Pointer ty',
                  ForeignFuncall ("malloc",
                                  [ArithOp (Arith.Modular,
                                            Arith.Mul,
                                            len',
                                            SizeOf ty')],
                                  Pointer ty'))
        end
      | transform (M.Free ptr) =
        ForeignFuncall ("free", [transform ptr], Unit)
      | transform (M.Load exp) =
        Load (transform exp)
      | transform (M.Store (ptr, value)) =
        Store (transform ptr, transform value)
      | transform (M.The (ty, exp)) =
        Cast (transformType ty, transform exp)
      | transform (M.Construct (ty, name, value)) =
        Construct (transformType ty,
                   caseNameIdx ty name,
                   Option.map transform value)
      | transform (M.Case (exp, cases, ty)) =
        let val expTy = MTAST.typeOf exp
        in
            let val expvar = freshVar ()
            in
                (* TODO: better name for this *)
                let val expvarVar = Variable (expvar, transformType ty)
                in
                    let fun transformCase (M.VariantCase (M.NameOnly name, body)) =
                            VariantCase (caseNameIdx expTy name, transform body)
                          | transformCase (M.VariantCase (M.NameBinding { casename, var, ty }, body)) =
                            let val id = caseNameIdx expTy casename
                            in
                                VariantCase (id,
                                             Let (var,
                                                  UnsafeExtractCase (expvarVar,
                                                                     id,
                                                                     transformType ty),
                                                  transform body))
                            end
                    in
                        Let (expvar,
                             transform exp,
                             Case (expvarVar,
                                   map transformCase cases,
                                   transformType ty))
                    end
                end
            end
        end
      | transform (M.ForeignFuncall (name, args, ty)) =
        ForeignFuncall (name, map transform args, transformType ty)
      | transform (M.NullPointer ty) =
        NullPointer (transformType ty)
      | transform (M.SizeOf ty) =
        SizeOf (transformType ty)
      | transform (M.AddressOf (var, ty)) =
        AddressOf (var, transformType ty)
      | transform (M.Cast (ty, exp)) =
        Cast (transformType ty, transform exp)
      | transform (M.Seq (a, b)) =
        Seq (transform a, transform b)
      | transform (M.ConcreteFuncall (name, args, ty)) =
        ConcreteFuncall (name, map transform args, transformType ty)
      | transform (M.GenericFuncall (name, id, args, ty)) =
        GenericFuncall (name,
                        id,
                        map transform args,
                        transformType ty)

    and transformBind tupty (vars: Symbol.variable list) (tupvar: Symbol.variable) (body: MTAST.ast) =
        let fun transformInner (head::tail) tupvar body i =
                Let (head,
                     TupleProj (Variable (tupvar, tupty), i),
                     transformInner tail tupvar body (i + 1))
              | transformInner nil _ body _ =
                transform body
        in
            transformInner vars tupvar body 0
        end

    fun transformTop (M.Defun (name, params, ty, body)) =
        Defun (name,
               mapParams params,
               transformType ty,
               transform body)
      | transformTop (M.DefunMonomorph (name, params, ty, body, id)) =
        DefunMonomorph (name,
                        mapParams params,
                        transformType ty,
                        transform body,
                        id)
      | transformTop (M.DefdatatypeMono (name, id, tys)) =
        DefdatatypeMono (name,
                         id,
                         map transformType tys)
      | transformTop (M.ToplevelProgn l) =
        ToplevelProgn (map transformTop l)

    and mapParams l =
        map mapParam l

    and mapParam (MTAST.Param (var, ty)) =
        Param (var, transformType ty)
end
