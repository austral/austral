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

structure TAST :> TAST = struct
    type name = Symbol.symbol
    type ty = Type.ty

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Bind of Symbol.variable list * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | CompOp of Builtin.comp_op * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Malloc of ty * ast
                 | Free of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | ForeignFuncall of string * ast list * ty
                 | ForeignNull of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | ConcreteFuncall of name * ast list * ty
                 | GenericFuncall of name * Type.typarams * (name, ty) Map.map * ast list * ty
                 | MethodFuncall of name * ty list * ast list * ty
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: Symbol.variable, ty: ty }

    type param_name = name
    type docstring = string option
    type symbol = Symbol.symbol
    type variable = Symbol.variable

    datatype top_ast = Defun of name * param list * ty * docstring * ast
                     | Defgeneric of name * Type.typarams * param list * ty * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * Type.typarams * docstring * ty
                     | Defdatatype of name * Type.typarams * docstring * Type.variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
                     | Defcfun of name * string * param list * Function.foreign_arity * ty * docstring
         and param = Param of variable * ty
         and method_decl = MethodDecl of name * param list * ty * docstring
         and method_def = MethodDef of name * param list * ty * docstring * ast
         and instance_arg = InstanceArg of name * Type.typarams

    local
        open Type
    in
        fun typeOf UnitConstant =
            Unit
          | typeOf (BoolConstant _) =
            Bool
          | typeOf (IntConstant (_, t)) =
            t
          | typeOf (FloatConstant (_, t)) =
            t
          | typeOf (StringConstant s) =
            StaticArray (Integer (Unsigned, Int8))
          | typeOf (Variable (_, t)) =
            t
          | typeOf (Let (_, _, b)) =
            typeOf b
          | typeOf (Bind (_, _, b)) =
            typeOf b
          | typeOf (Cond (_, tb, _)) =
            typeOf tb
          | typeOf (ArithOp (kind, _, lhs, _)) =
            (* The type of most arithmetic operations is the type of the
               arguments, except for checked operations, where the result is a
               tuple of the resulting value and a boolean indicating overflow *)
            (case kind of
                 Arith.Checked => Tuple [typeOf lhs, Bool]
               | _ => typeOf lhs)
          | typeOf (CompOp _) =
            Bool
          | typeOf (TupleCreate exps) =
            Tuple (map typeOf exps)
          | typeOf (TupleProj (tup, idx)) =
            (case typeOf tup of
                 Tuple tys => List.nth (tys, idx)
               | _ => raise Fail "Not a tuple")
          | typeOf (ArrayLength _) =
            Integer (Unsigned, Int64)
          | typeOf (ArrayPointer arr) =
            (case (typeOf arr) of
                 (StaticArray ty) => Address ty
               | _ => raise Fail "Invalid type for ArrayPointer")
          | typeOf (Malloc (t, _)) =
            Address t
          | typeOf (Free _) =
            Unit
          | typeOf (Load p) =
            (case typeOf p of
                 (PositiveAddress t) => t
               | _ => raise Fail "Not a positive address")
          | typeOf (Store (_, v)) =
            typeOf v
          | typeOf (The (t, _)) =
            t
          | typeOf (Construct (t, _, _)) =
            t
          | typeOf (Case (_, _, t)) =
            t
          | typeOf (SizeOf _) =
            Integer (Unsigned, Int64)
          | typeOf (AddressOf (_, ty)) =
            PositiveAddress ty
          | typeOf (Cast (ty, _)) =
            ty
          | typeOf (ForeignFuncall (_, _, rt)) =
            rt
          | typeOf (ForeignNull ty) =
            Address ty
          | typeOf (Seq (_, v)) =
            typeOf v
          | typeOf (ConcreteFuncall (_, _, ty)) =
            ty
          | typeOf (GenericFuncall (_, _, _, _, ty)) =
            ty
          | typeOf (MethodFuncall (_, _, _, ty)) =
            ty
    end

    (* Bindings *)

    datatype binding = Binding of ty * mutability
         and mutability = Immutable
                        | Mutable

    fun bindType (Binding (t, _)) = t

    type bindings = (Symbol.variable, binding) Map.map

    (* Context data for the augment function *)

    datatype context = Context of bindings * Type.tenv * Type.param Set.set * Function.fenv

    fun mkContext b t p f = Context (b, t, p, f)

    fun ctxBindings (Context (b, _, _, _)) = b
    fun ctxTenv (Context (_, t, _, _)) = t
    fun ctxTyParams (Context (_, _, ps, _)) = ps
    fun ctxFenv (Context (_, _, _, f)) = f

    (* Augment AST with type information *)

    local
        open Type
    in
        val defaultIntType = Integer (Signed, Int32)
        val sizeType = Integer (Unsigned, Int64)
        val defaultFloatType = Float Double

        fun augment AST.UnitConstant _ = UnitConstant
          | augment (AST.BoolConstant b) _ = BoolConstant b
          | augment (AST.IntConstant i) _ = IntConstant (i, defaultIntType)
          | augment (AST.FloatConstant f) _ = FloatConstant (f, defaultFloatType)
          | augment (AST.StringConstant s) _ = StringConstant s
          | augment (AST.Variable name) c =
            (case (Map.get (ctxBindings c) name) of
                 SOME bind => Variable (name, bindType bind)
               | NONE => raise Fail ("No such variable: " ^ (Symbol.varToString name)))
          | augment (AST.Let (name, v, body)) c =
            let val v' = augment v c
            in
                let val s' = Map.iadd (ctxBindings c)
                                      (name, (Binding (typeOf v', Mutable)))
                in
                    Let (name,
                         v',
                         augment body (mkContext s' (ctxTenv c) (ctxTyParams c) (ctxFenv c)))
                end
            end
          | augment (AST.Bind (vars, tup, body)) c =
            let val tup' = augment tup c
            in
                let fun augmentBind tys =
                        if (List.length tys) = (List.length vars) then
                            let val s' = Map.iaddList (ctxBindings c)
                                                      (Util.mapidx (fn (b, idx) => (b, Binding (List.nth (tys, idx), Immutable)))
                                                                   vars)
                            in
                                let val ctx' = mkContext s' (ctxTenv c) (ctxTyParams c) (ctxFenv c)
                                in
                                    Bind (vars, tup', augment body ctx')
                                end
                            end
                        else
                            raise Fail "Number of bindings does not match tuple size"
                in
                    case typeOf tup' of
                        (Tuple tys) => augmentBind tys
                     |  _ => raise Fail "Cannot bind a non-tuple"
                end
            end
          | augment (AST.Cond (test, cons, alt)) c =
            let val test' = augment test c
                and cons' = augment cons c
                and alt'  = augment alt c
            in
                if (typeOf test') <> Bool then
                    raise Fail "The test in an `if` must be of boolean type"
                else
                    if (typeOf cons') <> (typeOf alt') then
                        raise Fail "The consequent and the alternate must have the same type"
                    else
                        Cond (test', cons', alt')
            end
          | augment (AST.ArithOp (kind, oper, lhs, rhs)) c =
            let val lhs' = augment lhs c
                and rhs' = augment rhs c
            in
                let val lhsTy = typeOf lhs'
                    and rhsTy = typeOf rhs'
                in
                    if rhsTy = lhsTy then
                        case kind of
                            Arith.Float => if Type.isFloat lhsTy then
                                               ArithOp (kind, oper, lhs', rhs')
                                           else
                                               raise Fail "Argument must be a float"
                          | _ => if Type.isInteger lhsTy then
                                     ArithOp (kind, oper, lhs', rhs')
                                 else
                                     raise Fail "Argument must be an integer"
                    else
                        raise Fail "Both arguments to an arithmetic operator must be of the same type"
                end
            end
          | augment (AST.CompOp (oper, lhs, rhs)) c =
            let val lhs' = augment lhs c
                and rhs' = augment rhs c
            in
                let val lhsTy = typeOf lhs'
                    and rhsTy = typeOf rhs'
                in
                    if lhsTy = rhsTy then
                        if Type.isComparable lhsTy then
                            CompOp (oper, lhs', rhs')
                        else
                            raise Fail "Both arguments to a comparison operation must be comparable"
                    else
                        raise Fail "Both arguments to a comparison operator must be of the same type"
                end
            end
          | augment (AST.TupleCreate exps) c =
            TupleCreate (map (fn e => augment e c) exps)
          | augment (AST.TupleProj (exp, i)) c =
            TupleProj (augment exp c, i)
          | augment (AST.StaticArrayLength arr) c =
            let val arr' = augment arr c
            in
                case typeOf arr' of
                    (StaticArray _) => ArrayLength arr'
                  | _ => raise Fail "Argument to static-array-length not a static array"

            end
          | augment (AST.Malloc (ty, len)) c =
            let val tenv = ctxTenv c
            in
                let val ty' = resolve tenv (ctxTyParams c) ty
                in
                    let val len' = augment len c
                    in
                        Malloc (ty', len')
                    end
                end
            end
          | augment (AST.Free ptr) c =
            let val ptr' = augment ptr c
            in
                Free ptr'
            end
          | augment (AST.Load e) c =
            let val e' = augment e c
            in
                case (typeOf e') of
                    PositiveAddress t => Load e'
                  | _ => raise Fail "load: not a positive address"
            end
          | augment (AST.Store (p, v)) c =
            let val p' = augment p c
                and v' = augment v c
            in
                case (typeOf p') of
                    PositiveAddress t => let val ty = typeOf v'
                                         in
                                             if ty = t then
                                                 Store (p', v')
                                             else
                                                 raise Fail "store: type mismatch"
                                         end
                  | _ => raise Fail "store: first argument must be a positive address"
            end
          | augment (AST.The (typespec, exp)) c =
            let val tenv = ctxTenv c
            in
                let val ty = resolve tenv (ctxTyParams c) typespec
                in
                    case exp of
                        (AST.IntConstant i) => if isInteger ty then
                                                   The (ty, augment exp c)
                                               else
                                                   raise Fail "Bad types for `the`"
                      | (AST.FloatConstant f) => if isFloat ty then
                                                     The (ty, augment exp c)
                                                 else
                                                     raise Fail "Bad types for `the`"
                      | (AST.Funcall (name, args)) => augmentFuncall name args c (SOME ty)
                      | e => let val exp' = augment exp c
                             in
                                 if ty = typeOf exp' then
                                     The (ty, exp')
                                 else
                                     raise Fail "Bad types for `the`"
                             end
                end
            end
          | augment (AST.Construct (typespec, label, exp)) c =
            (* Three things: verify that the type is indeed a datatype,
               verify that the label is a valid variant name, and check that the
               expression (if the variant carries a value) is of the correct
               type. *)
            let val ty = resolve (ctxTenv c) (ctxTyParams c) typespec
            in
                case ty of
                    (Disjunction (name, tyargs, variants)) =>
                    (case getVariantByName variants label of
                         (SOME (Variant (_, tyOpt))) =>
                         (case tyOpt of
                              (SOME caseTy) => (case exp of
                                                    (SOME exp') =>
                                                    let val exp'' = augment exp' c
                                                    in
                                                        if typeOf exp'' = caseTy then
                                                            Construct (ty, label, SOME exp'')
                                                        else
                                                            raise Fail "construct: type mismatch"
                                                    end
                                                  | NONE => raise Fail "construct: missing value")
                            | NONE => (case exp of
                                           (SOME _) => raise Fail "construct: superfluous value"
                                         | NONE => Construct (ty, label, NONE)))
                       | _ => raise Fail ("construct: not a valid variant name: "
                                          ^ (Symbol.toString label)
                                          ^ ". Valid variant names for this type: \n  "
                                          ^ (String.concatWith "\n  " (map (fn (Variant (name, _)) => Symbol.toString name) variants))
                                          ^ "\n"))
                  | _ => raise Fail "construct: not a datatype"
            end
          | augment (AST.Case (exp, cases)) c =
            (*
               Things we have to verify:

               1. The type of exp is a datatype.

               2. The set of case names in this expression is equal to the set
                  of case names in the datatype.

               3. If a case name is a name without a binding, the corresponding
                  case in the datatype must not have a value associated to
                  it.

               4. If a case name has a binding, the corresponding case in the
                  datatype must have a value associated to it.

               5. All cases have the same type.
             *)
            let val exp' = augment exp c
            in
                case typeOf exp' of
                    (Disjunction (_, tyargs, variants)) =>
                    let val caseNames = map (fn (AST.VariantCase (name, _)) =>
                                                case name of
                                                    (AST.NameOnly n) => n
                                                 | (AST.NameBinding { casename = n, var = _ }) => n)
                                            cases
                        and variantNames = map (fn (Type.Variant (name, _)) => name) variants
                    in
                        if Set.eq (Set.fromList caseNames) (Set.fromList variantNames) then
                            let fun transformCase (AST.VariantCase (name, body)) =
                                    (case name of
                                         (AST.NameOnly name') =>
                                         (case getVariantByName variants name' of
                                              (SOME variant) =>
                                              (* Since this is a name-only case,
                                                 the variant must have no associated
                                                 value *)
                                              (case variant of
                                                   (Type.Variant (_, NONE)) => VariantCase (NameOnly name',
                                                                                            augment body c)
                                                 | _ => raise Fail "case: this case has no binding, but the associated variant has an associated value")
                                            | _ => raise Fail "no variant with this name")
                                       | (AST.NameBinding { casename = casename, var = var }) =>
                                         (case getVariantByName variants casename of
                                              (SOME variant) =>
                                              (* Since this case has a binding,
                                                 the variant must have an
                                                 associated value *)
                                              (case variant of
                                                   (Type.Variant (_, SOME ty)) =>
                                                   let val s' = Map.iadd (ctxBindings c)
                                                                         (var, (Binding (ty, Immutable)))
                                                   in
                                                       let val c' = mkContext s' (ctxTenv c) (ctxTyParams c) (ctxFenv c)
                                                       in
                                                           VariantCase (NameBinding { casename = casename, var = var, ty = ty },
                                                                        augment body c')
                                                       end
                                                   end
                                                 | _ => raise Fail "case: this case has no binding, but the associated variant has an associated value")
                                            | _ => raise Fail "no variant with this name"))

                                     and caseType (VariantCase (_, body)) =
                                         typeOf body
                            in
                                let val cases' = map transformCase cases
                                in
                                    if (Set.size (Set.fromList (map (fn (VariantCase (_, b)) => typeOf b) cases'))) = 1 then
                                        Case (exp',
                                              cases',
                                              caseType (List.hd cases'))
                                    else
                                        raise Fail "case: not all cases have the same type"
                                end
                            end
                        else
                            raise Fail "case: the set of case names in the datatype is not equal to the set of case names in the expression"
                    end
                  | _ => raise Fail "case: the type of the expression is not a datatype"
            end
          | augment (AST.SizeOf typespec) c =
            SizeOf (resolve (ctxTenv c) (ctxTyParams c) typespec)
          | augment (AST.AddressOf name) c =
            (case (Map.get (ctxBindings c) name) of
                 (SOME (Binding (ty, Mutable))) => AddressOf (name, Address ty)
               | (SOME (Binding (ty, Immutable))) => raise Fail ("address-of: the variable " ^ (Symbol.varToString name) ^ " must be mutable to take its address.")
               | NONE => raise Fail ("No such variable: " ^ (Symbol.varToString name)))
          | augment (AST.ForeignNull typespec) c =
            ForeignNull (resolve (ctxTenv c) (ctxTyParams c) typespec)
          | augment (AST.Cast (typespec, exp)) c =
            let val ty = (resolve (ctxTenv c) (ctxTyParams c) typespec)
                and exp' = augment exp c
            in
                let fun compatible (Type.Integer _) (Type.Integer _) =
                        true
                      | compatible (Type.Float Type.Single) (Type.Float Type.Single) =
                        true
                      | compatible (Type.Float Type.Double) (Type.Float Type.Double) =
                        true
                      | compatible (Type.Address _) (Type.Address _) =
                        true
                      | compatible (Type.StaticArray _) (Type.StaticArray _) =
                        true
                      | compatible _ _ =
                        false
                in
                    if compatible ty (typeOf exp') then
                        Cast (ty, exp')
                    else
                        raise Fail "cast: casted-to type and type of expression are not compatible"
                end
            end
          | augment (AST.Seq (a, b)) c =
            Seq (augment a c,
                 augment b c)
          | augment (AST.Funcall (name, args)) c =
            augmentFuncall name args c NONE

        and augmentFuncall name args c the_context =
            let val fenv = ctxFenv c
                and auKer = Symbol.auKer
            in
                case Function.envGet fenv name of
                    SOME f => augmentCallable f args c the_context
                  | NONE => raise Fail ("No function with this name: " ^ (Symbol.toString name))
            end

        and augmentCallable (Function.CallableFunc f) args c _ =
            augmentConcreteFuncall f args c
          | augmentCallable (Function.CallableForeign ff) args c _ =
            augmentForeignFuncall ff args c
          | augmentCallable (Function.CallableGFunc gf) args c the_context =
            if Function.isRTP gf then
                augmentRTPGenericFuncall gf args c the_context
            else
                augmentGenericFuncall gf args c
          | augmentCallable Function.CallableMethod args c the_context =
            augmentMethodCall args c the_context

        and augmentConcreteFuncall (Function.Function (name, params, rt, _)) args c =
            if (List.length params) = (List.length args) then
                ConcreteFuncall (name,
                                 map (fn a => augment a c) args,
                                 rt)
            else
                raise Fail "Funcall arity error"

        and augmentForeignFuncall ff args c =
            let val tenv = ctxTenv c
                and (Function.ForeignFunction (_, rawname, params, arity, rt, _)) = ff
            in
                let fun augmentArg value =
                        let val value' = augment value c
                        in
                            if validType (typeOf value') then
                                case typeOf value' of
                                    (StaticArray t) => ArrayPointer value'
                                  | _ => value'
                            else
                                raise Fail "Type is not valid for a foreign funcall"
                        end

                    and validType (Type.Integer _) = true
                      | validType (Type.Float _) = true
                      | validType (Type.Address _) = true
                      | validType (Type.StaticArray _) = true
                      | validType _ = false

                    and checkArgs arglist =
                        case arity of
                            Function.FixedArity =>
                            (* If the function has a fixed arity, we have to
                               check that the parameter list and the argument
                               list have the same length, and then check that
                               each argument has the same type as its
                               corresponding parameter *)
                            if (List.length arglist) = (List.length params) then
                                ListPair.map checkArg (arglist, params)
                            else
                                raise Fail "Foreign funcall arity error"
                          | Function.VariableArity =>
                            (* If the function has a variable arity, we divide
                               the argument list into two segments: the first is
                               as long as the parameter list, and the second is
                               the remainder of that. Then, we check that the
                               arguments in the first list have types that
                               correspond to those in the parameter list. For
                               the second list, we don't do type checking. *)
                            let val paramCount = List.length params
                            in
                                if (List.length arglist) >= paramCount then
                                    let val firstArgs = List.take (arglist, paramCount)
                                        and secondArgs = List.drop (arglist, paramCount)
                                    in
                                        let val firstArgs' = ListPair.map checkArg (firstArgs, params)
                                        in
                                            firstArgs' @ secondArgs
                                        end
                                    end
                                else
                                    raise Fail "Not enough arguments"
                            end

                    and checkArg (argument, (Function.Param (_, ty))) =
                        if (typeOf argument) <> ty then
                            raise Fail "Foreign funcall: type mismatch"
                        else
                            argument
                in

                    ForeignFuncall (rawname,
                                    checkArgs (map augmentArg args),
                                    rt)
                end
            end

        and augmentGenericFuncall gf args c =
            let val (Function.GenericFunction (_, _, _, rt, _)) = gf
            in
                augmentGenericFuncallCommon gf args c rt
            end

        and augmentRTPGenericFuncall gf args c the_context =
            (case the_context of
                 SOME ty => augmentGenericFuncallCommon gf args c ty
               | NONE => raise Fail ("Error in call to function "
                                     ^ (Symbol.toString (Function.gFunctionName gf))
                                     ^ ": generic functions that are return-type polymorphic must be called in the context of a `the` form."))

        and augmentGenericFuncallCommon gf args c assertedRt =
            let val (Function.GenericFunction (name, typarams, params, rt, _)) = gf
            in
                if (List.length params) = (List.length args) then
                    let val args' = map (fn a => augment a c) args
                    in
                        let val argTypes = map typeOf args'
                        in
                            let val binds = Function.matchFunc params rt argTypes assertedRt
                            in
                                GenericFuncall (name,
                                                typarams,
                                                binds,
                                                args',
                                                assertedRt)
                            end
                        end
                    end
                else
                    raise Fail "Funcall arity error"
            end

        and augmentMethodCall args c the_context =
            raise Fail "method calls not implemented yet"
    end

    fun funcContext params typarams tenv fenv =
        let val bindings = Map.fromList (map (fn (DAST.Param (var, ty)) => (var, Binding (ty, Immutable)))
                                             params)
        in
            Context (bindings, tenv, typarams, fenv)
        end

    fun mapParam (DAST.Param (n, ty)) =
        Param (n, ty)

    fun mapParams ps =
        map mapParam ps

    fun augmentTop (DAST.Defun (name, params, ty, docstring, ast)) tenv fenv =
        Defun (name,
               mapParams params,
               ty,
               docstring,
               augment ast (funcContext params Set.empty tenv fenv))
      | augmentTop (DAST.Defgeneric (name, typarams, params, ty, docstring, ast)) tenv fenv =
        Defgeneric (name,
                    typarams,
                    mapParams params,
                    ty,
                    docstring,
                    augment ast (funcContext params (OrderedSet.toUnordered typarams) tenv fenv))
      | augmentTop (DAST.Defclass (name, paramName, docstring, methods)) tenv fenv =
        let fun augmentMethod (DAST.MethodDecl (name, params, ty, docstring)) =
                MethodDecl (name,
                            mapParams params,
                            ty,
                            docstring)
        in
            Defclass (name, paramName, docstring, map augmentMethod methods)
        end
      | augmentTop (DAST.Definstance (name, DAST.InstanceArg (arg, typarams), docstring, defs)) tenv fenv =
        let fun mapDef (DAST.MethodDef (name, params, ty, docstring, ast)) =
                MethodDef (name,
                           mapParams params,
                           ty,
                           docstring,
                           augment ast (funcContext params (OrderedSet.toUnordered typarams) tenv fenv))
        in
            Definstance (name, InstanceArg (arg, typarams), docstring, map mapDef defs)
        end
      | augmentTop (DAST.Deftype (name, params, docstring, ty)) tenv _ =
            Deftype (name,
                     params,
                     docstring,
                     ty)
      | augmentTop (DAST.Defdatatype (name, params, docstring, variants)) tenv _ =
        Defdatatype (name,
                     params,
                     docstring,
                     variants)
      | augmentTop (DAST.Deftemplate tmpl) _ _ =
        Deftemplate tmpl
      | augmentTop (DAST.DefineSymbolMacro (name, exp, docstring)) _ _ =
        DefineSymbolMacro (name, exp, docstring)
      | augmentTop (DAST.Defmodule clauses) _ _ =
        Defmodule clauses
      | augmentTop (DAST.InModule name) _ _ =
        InModule name
      | augmentTop (DAST.Defcfun (name, rawname, params, arity, rt, docstring)) _ _ =
        Defcfun (name,
                 rawname,
                 mapParams params,
                 arity,
                 rt,
                 docstring)
end
