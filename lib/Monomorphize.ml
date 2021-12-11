open Identifier
open Type
open MonoType
open Tast
open Mtast
open Error

type stripped_ty =
  | SUnit
  | SBoolean
  | SInteger of signedness * integer_width
  | SSingleFloat
  | SDoubleFloat
  | SNamedType of qident * stripped_ty list
  | SArray of stripped_ty
  | SReadRef of stripped_ty
  | SWriteRef of stripped_ty
  | SRawPointer of stripped_ty

let rec strip_type (ty: ty): stripped_ty =
  match strip_type' ty with
  | Some ty ->
     ty
  | None ->
     err "strip_type called with a region type as its argument"

and strip_type' (ty: ty): stripped_ty option =
  match ty with
  | Unit ->
     Some SUnit
  | Boolean ->
     Some SBoolean
  | Integer (s, w) ->
     Some (SInteger (s, w))
  | SingleFloat ->
     Some SSingleFloat
  | DoubleFloat ->
     Some SDoubleFloat
  | NamedType (n, args, _) ->
     Some (SNamedType (n, List.filter_map strip_type' args))
  | Array (elem_ty, _) ->
     (match (strip_type' elem_ty) with
      | Some elem_ty ->
         Some (SArray elem_ty)
      | None ->
         err "Internal: array instantiated with a region type.")
  | RegionTy _ ->
     None
  | ReadRef (ty, _) ->
     (match (strip_type' ty) with
      | Some ty ->
         Some (SReadRef ty)
      | None ->
         err "Internal: read ref instantiated with a region type.")
  | WriteRef (ty, _) ->
     (match (strip_type' ty) with
      | Some ty ->
         Some (SWriteRef ty)
      | None ->
         err "Internal: write ref instantiated with a region type.")
  | TyVar _ ->
     (* Why? Because when instantiating a monomorph, we do search and replace of
        type variables with their substitutions. So if there are variables left
        over by stripping time, that's an error. Anyways, the search-and-replace
        step should *also* have signalled an error if a type variable has no
        replacement. *)
     err "Variable not replaced."
  | RawPointer ty ->
     (match (strip_type' ty) with
      | Some ty ->
         Some (SRawPointer ty)
      | None ->
         err "Internal: raw pointer type instantiated with a region type.")

let rec monomorphize_type (tbl: mono_tbl) (ty: stripped_ty): (mono_ty * mono_tbl) =
  match ty with
  | SUnit ->
     (MonoUnit, tbl)
  | SBoolean ->
     (MonoBoolean, tbl)
  | SInteger (s, w) ->
     (MonoInteger (s, w), tbl)
  | SSingleFloat ->
     (MonoSingleFloat, tbl)
  | SDoubleFloat ->
     (MonoDoubleFloat, tbl)
  | SArray elem_ty ->
     let (elem_ty, tbl) = monomorphize_type tbl elem_ty in
     (MonoArray elem_ty, tbl)
  | SReadRef ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoReadRef ty, tbl)
  | SWriteRef ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoWriteRef ty, tbl)
  | SRawPointer ty ->
     let (ty, tbl) = monomorphize_type tbl ty in
     (MonoRawPointer ty, tbl)
  | SNamedType (name, args) ->
     let (args, tbl) = monomorphize_ty_list tbl args in
     (match get_monomorph_id tbl name args with
      | Some id ->
         (MonoNamedType (name, id), tbl)
      | None ->
         let (id, tbl) = add_monomorph tbl name args in
         (MonoNamedType (name, id), tbl))

and monomorphize_ty_list (tbl: mono_tbl) (tys: stripped_ty list): (mono_ty list * mono_tbl) =
  match tys with
  | first::rest ->
     let (first, tbl) = monomorphize_type tbl first in
     let (rest, tbl) = monomorphize_ty_list tbl rest in
     (first :: rest, tbl)
  | [] ->
     ([], tbl)

let rec monomorphize_expr (tbl: mono_tbl) (expr: texpr): (mexpr * mono_tbl) =
  match expr with
  | TNilConstant ->
     (MNilConstant, tbl)
  | TBoolConstant b ->
     (MBoolConstant b, tbl)
  | TIntConstant i ->
     (MIntConstant i, tbl)
  | TFloatConstant f ->
     (MFloatConstant f, tbl)
  | TStringConstant s ->
     (MStringConstant s, tbl)
  | TVariable (name, ty) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     (MVariable (name, ty), tbl)
  | TArithmetic (oper, lhs, rhs) ->
     let (lhs, tbl) = monomorphize_expr tbl lhs in
     let (rhs, tbl) = monomorphize_expr tbl rhs in
     (MArithmetic (oper, lhs, rhs), tbl)
  | TFuncall (name, args, rt, substs) ->
     (* Monomorphize the return type. *)
     let rt = strip_type rt in
     let (rt, tbl) = monomorphize_type tbl rt in
     (* Monomorphize the arglist *)
     let (args, tbl) = monomorphize_expr_list tbl args in
     (* Does the funcall have a substitution list? *)
     if List.length substs > 0 then
       (* The function is generic. *)
       (* Monomorphize the tyargs *)
       let tyargs = List.map (fun (_, ty) -> strip_type ty) substs in
       let (tyargs, tbl) = monomorphize_ty_list tbl tyargs in
       (match get_monomorph_id tbl name tyargs with
        | Some id ->
           (MGenericFuncall (id, args, rt), tbl)
        | None ->
           let (id, tbl) = add_monomorph tbl name tyargs in
           (MGenericFuncall (id, args, rt), tbl))
     else
       (* The function is concrete. *)
       (MConcreteFuncall (name, args, rt), tbl)
  | TMethodCall (name, STypeClassInstance (_, _, typarams, _, _), args, rt, substs) ->
     (* Monomorphize the return type. *)
     let rt = strip_type rt in
     let (rt, tbl) = monomorphize_type tbl rt in
     (* Monomorphize the arglist *)
     let (args, tbl) = monomorphize_expr_list tbl args in
     (* Does the funcall have a list of type params? *)
     if List.length typarams > 0 then
       (* The instance is generic. *)
       (* Monomorphize the tyargs *)
       let tyargs = List.map (fun (_, ty) -> strip_type ty) substs in
       let (tyargs, tbl) = monomorphize_ty_list tbl tyargs in
       (match get_monomorph_id tbl name tyargs with
        | Some id ->
           (MGenericFuncall (id, args, rt), tbl)
        | None ->
           let (id, tbl) = add_monomorph tbl name tyargs in
           (MGenericFuncall (id, args, rt), tbl))
     else
       (* The instance is concrete. *)
       (MConcreteFuncall (name, args, rt), tbl)
  | TCast (expr, ty) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (expr, tbl) = monomorphize_expr tbl expr in
     (MCast (expr, ty), tbl)
  | TComparison (oper, lhs, rhs) ->
     let (lhs, tbl) = monomorphize_expr tbl lhs in
     let (rhs, tbl) = monomorphize_expr tbl rhs in
     (MComparison (oper, lhs, rhs), tbl)
  | TConjunction (lhs, rhs) ->
     let (lhs, tbl) = monomorphize_expr tbl lhs in
     let (rhs, tbl) = monomorphize_expr tbl rhs in
     (MConjunction (lhs, rhs), tbl)
  | TDisjunction (lhs, rhs) ->
     let (lhs, tbl) = monomorphize_expr tbl lhs in
     let (rhs, tbl) = monomorphize_expr tbl rhs in
     (MDisjunction (lhs, rhs), tbl)
  | TNegation expr ->
     let (expr, tbl) = monomorphize_expr tbl expr in
     (MNegation expr, tbl)
  | TIfExpression (c, t, f) ->
     let (c, tbl) = monomorphize_expr tbl c in
     let (t, tbl) = monomorphize_expr tbl t in
     let (f, tbl) = monomorphize_expr tbl f in
     (MIfExpression (c, t, f), tbl)
  | TRecordConstructor (ty, args) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (args, tbl) = monomorphize_named_expr_list tbl args in
     (MRecordConstructor (ty, args), tbl)
  | TUnionConstructor (ty, case_name, args) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (args, tbl) = monomorphize_named_expr_list tbl args in
     (MUnionConstructor (ty, case_name, args), tbl)
  | TPath { head; elems; ty } ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (head, tbl) = monomorphize_expr tbl head in
     let (elems, tbl) = monomorphize_path_elems tbl elems in
     (MPath { head = head; elems = elems; ty = ty }, tbl)
  | TEmbed (ty, fmt, args) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (args, tbl) = monomorphize_expr_list tbl args in
     (MEmbed (ty, fmt, args), tbl)
  | TDeref expr ->
     let (expr, tbl) = monomorphize_expr tbl expr in
     (MDeref expr, tbl)
  | TSizeOf ty ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     (MSizeOf ty, tbl)

and monomorphize_expr_list (tbl: mono_tbl) (exprs: texpr list): (mexpr list * mono_tbl) =
  match exprs with
  | first::rest ->
     let (first, tbl) = monomorphize_expr tbl first in
     let (rest, tbl) = monomorphize_expr_list tbl rest in
     (first :: rest, tbl)
  | [] ->
     ([], tbl)

and monomorphize_named_expr_list (tbl: mono_tbl) (exprs: (identifier * texpr) list): ((identifier * mexpr) list * mono_tbl) =
  match exprs with
  | (name, first)::rest ->
     let (first, tbl) = monomorphize_expr tbl first in
     let (rest, tbl) = monomorphize_named_expr_list tbl rest in
     ((name, first) :: rest, tbl)
  | [] ->
     ([], tbl)

and monomorphize_path_elems (tbl: mono_tbl) (elems: typed_path_elem list): (mtyped_path_elem list * mono_tbl) =
  match elems with
  | first::rest ->
     let (first, tbl) = monomorphize_path_elem tbl first in
     let (rest, tbl) = monomorphize_path_elems tbl rest in
     (first :: rest, tbl)
  | [] ->
     ([], tbl)

and monomorphize_path_elem (tbl: mono_tbl) (elem: typed_path_elem): (mtyped_path_elem * mono_tbl) =
  match elem with
  | TSlotAccessor (name, ty) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     (MSlotAccessor (name, ty), tbl)
  | TPointerSlotAccessor (name, ty) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     (MPointerSlotAccessor (name, ty), tbl)
  | TArrayIndex (idx, ty) ->
     let ty = strip_type ty in
     let (ty, tbl) = monomorphize_type tbl ty in
     let (idx, tbl) = monomorphize_expr tbl idx in
     (MArrayIndex (idx, ty), tbl)
