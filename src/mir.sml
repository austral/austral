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

structure MIR :> MIR = struct
    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of string
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Cast of Type.typespec * ast
                 | Funcall of string * ast list

    datatype block_ast = Let of string * ast * ast
                       | Cond of ast * ast * ast
                       | Progn of ast list

    type typespec = Type.typespec

    datatype top_ast = DefunConcrete of string * (string * typespec) list * typespec * block_ast * ast

    fun transform _ = raise Fail "derp"

    (*local
        open CppAst
    in
        val unitConstant = BoolConstant false

        fun transform (AST.ConstInt i) =
            (Sequence [], ConstInt i)
          | transform (AST.ConstString s) _ =
            (Sequence [], ConstString s)
          | transform (AST.Var s) _ =
            (Sequence [], ngVar s)
          | transform (TBinop (oper, a, b, t)) =
            let val (ablock, aval) = convert a
                and (bblock, bval) = convert b
            in
                (Sequence [
                      ablock,
                      bblock
                  ],
                 Binop (oper, aval, bval))
            end
          | transform (TCond (t, c, a, _)) =
            let val (tblock, tval) = convert t
                and (cblock, cval) = convert c
                and (ablock, aval) = convert a
                and result = freshVar ()
                and resType = convertType (TAST.typeOf c)
            in
                (Sequence [
                      tblock,
                      Declare (resType, result),
                      Cond (tval,
                            Block [
                                cblock,
                                Assign (Var result, cval)
                            ],
                            Block [
                                ablock,
                                Assign (Var result, aval)
                           ])
                  ],
                 Var result)
            end
          | transform (TCast (ty, a)) =
            let val (ablock, aval) = convert a
            in
                (ablock, Cast (convertType ty, aval))
            end
          | transform (TProgn exps) =
            let val exps' = map (fn e => convert e) exps
            in
                if (length exps = 0) then
                    (Sequence [], unitConstant)
                else
                    (Sequence (map (fn (b, _) => b) exps'),
                     let val (_, v) = List.last exps' in v end)
            end
          | transform (TLet (name, v, b)) =
            let val (vblock, vval) = convert v
                and ty = convertType (typeOf v)
                and (bblock, bval) = convert b
            in
                (Block [vblock, Declare (ty, varName name), Assign (ngVar name, vval), bblock],
                 bval)
            end
          | transform (TBind (binds, tup, body)) =
            let val (tblock, tval) = convert tup
                and (bblock, bval) = convert body
            in
                case typeOf tup of
                    (Type.Tuple tupTys) =>
                    let val decls = ListPair.map (fn (n, t) => Declare (convertType t, varName n))
                                                 (binds, tupTys)
                        and assigns = ListPair.map (fn (n, idx) => Assign (ngVar n, AccessTuple (tval, idx)))
                                                   (binds, List.tabulate (List.length tupTys, fn x => x))
                    in
                        (Block (tblock :: decls @ assigns @ [bblock]), bval)
                    end
                  | _ => raise Fail "Not a tuple"
            end
          | transform (TAssign (var, v)) =
            let val (vblock, vval) = convert v
            in
                (Sequence [vblock, Assign (ngVar var, vval)], vval)
            end
          | transform (TTuple exps) =
            let val args = map (fn e => convert e) exps
            in
                (Sequence (map (fn (b, _) => b) args),
                 CreateTuple (map (fn (_, e) => e) args))
            end
          | transform (TTupleProj (exp, i)) =
            let val (vblock, vval) = convert exp
            in
                (vblock, AccessTuple (vval, i))
            end
          | transform (TNullPtr _) =
            (Sequence [], ConstNull)
          | transform (TLoad (e, _)) =
            let val (eblock, eval) = convert e
            in
                (eblock, Deref eval)
            end
          | transform (TStore (p, v)) =
            let val (pblock, pval) = convert p
                and (vblock, vval) = convert v
            in
                (Sequence [pblock, vblock, Assign ((Deref pval), vval)], vval)
            end
          | transform (TMalloc (t, c)) =
            let val (cblock, cval) = convert c
                and ty = convertType t
                and res = freshVar ()
            in
                let val sizecalc = Binop (Binop.Mul, cval, SizeOf ty)
                in
                    (Sequence [cblock, Declare (Pointer ty, res), Funcall (SOME res, "malloc", [sizecalc])],
                     Cast (Pointer ty, Var res))
                end
            end
          | transform (TFree p) =
            let val (pblock, pval) = convert p
            in
                (Sequence [pblock, Funcall (NONE, "free", [pval])], unitConstant)
            end
          | transform (TAddressOf (v, _)) _ =
            (Sequence [], AddressOf (ngVar v))
          | transform (TPrint (v, n)) =
            let val (vblock, vval) = convert v
                and ty = typeOf v
            in
                let val printer = if ty = Type.Bool then
                                      let val nl = (case n of
                                                        OAST.Newline => ConstBool true
                                                      | OAST.NoNewline => ConstBool false)
                                      in
                                          Funcall (NONE, "interim_print_bool", [vval, nl])
                                      end
                                  else
                                      Funcall (NONE, "printf", (formatStringFor ty n) @ [vval])
                in
                    (Sequence [vblock, printer],
                     unitConstant)
                end
            end
          | transform (TCEmbed (t, s)) =
            (Sequence [], Cast (convertType t, Raw s))
          | transform (TCCall (f, t, args)) =
            let val args' = map (fn a => convert a) args
                and t' = convertType t
            in
                let val blocks = map (fn (b, _) => b) args'
                    and argvals = map (fn (_, v) => v) args'
                in
                    if t = Type.Unit then
                        (Sequence (blocks @ [Funcall (NONE, f, argvals)]),
                         unitConstant)
                    else
                        let val res = freshVar ()
                        in
                            (Sequence (blocks @ [Declare (t', res), Funcall (SOME res, f, argvals)]),
                             Var res)
                        end
                end
            end
          | transform (TWhile (t, b)) =
            let val (tblock, tval) = convert t
                and (bblock, _) = convert b
            in
                (Sequence [tblock, While (tval, bblock)], unitConstant)
            end
          | transform (TFuncall (f, args, rt)) =
            let val args' = map (fn a => convert a) args
                and rt' = convertType rt
                and res = freshVar ()
            in
                let val blocks = map (fn (b, _) => b) args'
                    and argvals = map (fn (_, v) => v) args'
                in
                    (Sequence (blocks @ [Declare (rt', res), Funcall (SOME res, f, argvals)]),
                     Var res)
                end
            end
    end*)

    fun transformTop _ =
        raise Fail "derp"
end
