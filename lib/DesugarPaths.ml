(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Common
open Stages
open Error

let rec desugar_elem (elem: Ast.path_elem) (subpath: AstDP.aexpr) (is_ref_transform: bool): AstDP.aexpr =
  match elem with
  | Ast.SlotAccessor name ->
     AstDP.SlotAccessor (subpath, name)
  | Ast.PointerSlotAccessor name ->
     if is_ref_transform then
       AstDP.SlotAccessor (subpath, name)
     else
       AstDP.PointerSlotAccessor (subpath, name)
  | Ast.ArrayIndex expr ->
     let expr = transform_expr expr in
     AstDP.ArrayIndex (subpath, expr)

and desugar_path (head: qident) (exprs: Ast.path_elem list) (mode: borrowing_mode) (is_ref_transform: bool): AstDP.aexpr =
  desugar_path_inner head (List.rev exprs) mode is_ref_transform

and desugar_path_inner (head: qident) (exprs: Ast.path_elem list) (mode: borrowing_mode) (is_ref_transform: bool): AstDP.aexpr =
  match exprs with
  | [] ->
     if is_ref_transform then
       (* Base case: do nothing. *)
       AstDP.Variable head
     else
       (* Base case: borrow the head. *)
       AstDP.BorrowExpr (mode, head)
  | first::rest ->
     let rest: AstDP.aexpr = desugar_path_inner head rest mode is_ref_transform in
     let first: AstDP.aexpr = desugar_elem first rest is_ref_transform in
     first

and transform_expr (expr: Ast.aexpr): AstDP.aexpr =
  match expr with
  (* Transform paths *)
  | Ast.Path (head, path_exprs) ->
     let path = desugar_path head path_exprs ReadBorrow false in
     AstDP.Deref path
  | Ast.RefPath (head, path_exprs) ->
     desugar_path head path_exprs ReadBorrow true
  (* Everything else is the identity transform. *)
  | Ast.NilConstant ->
     AstDP.NilConstant
  | Ast.BoolConstant b ->
     AstDP.BoolConstant b
  | Ast.IntConstant s ->
     AstDP.IntConstant s
  | Ast.FloatConstant s ->
     AstDP.FloatConstant s
  | Ast.StringConstant es ->
     AstDP.StringConstant es
  | Ast.Variable q ->
     AstDP.Variable q
  | Ast.FunctionCall (q, al) ->
     AstDP.FunctionCall (q, transform_arglist al)
  | Ast.ArithmeticExpression (ao, e1, e2) ->
     AstDP.ArithmeticExpression (ao, transform_expr e1, transform_expr e2)
  | Ast.Comparison (co, e1, e2) ->
     AstDP.Comparison (co, transform_expr e1, transform_expr e2)
  | Ast.Conjunction (e1, e2) ->
     AstDP.Conjunction (transform_expr e1, transform_expr e2)
  | Ast.Disjunction (e1, e2) ->
     AstDP.Disjunction (transform_expr e1, transform_expr e2)
  | Ast.Negation e ->
     AstDP.Negation (transform_expr e)
  | Ast.IfExpression (e1, e2, e3) ->
     AstDP.IfExpression (transform_expr e1, transform_expr e2, transform_expr e3)
  | Ast.Embed (qt, s, el) ->
     AstDP.Embed (qt, s, List.map transform_expr el)
  | Ast.Deref e ->
     AstDP.Deref (transform_expr e)
  | Ast.Typecast (e, qt) ->
     AstDP.Typecast (transform_expr e, qt)
  | Ast.SizeOf qt ->
     AstDP.SizeOf qt
  | Ast.BorrowExpr (bm, q) ->
     AstDP.BorrowExpr (bm, q)
  | Ast.Reborrow q ->
     AstDP.Reborrow q

and transform_lvalue (expr: Ast.aexpr): AstDP.aexpr =
  match expr with
  (* Transform paths *)
  | Ast.Path (head, path_exprs) ->
     desugar_path head path_exprs WriteBorrow false
  (* Everything else is not a valid lvalue. *)
  | _ ->
     err "Invalid lvalue."

and transform_stmt (stmt: Ast.astmt): AstDP.astmt =
  match stmt with
  | Ast.ASkip span ->
     AstDP.ASkip span
  | Ast.ALet (span, m, id, qt, s) ->
     AstDP.ALet (span, m, id, qt, transform_stmt s)
  | Ast.ADestructure (span, m, qbl, e, s) ->
     AstDP.ADestructure (span, m, qbl, transform_expr e, transform_stmt s)
  | Ast.AAssign (span, lv, e) -> begin
      match lv with
      | Ast.Path (name, []) ->
         AstDP.AAssignVar (span, name, transform_expr e)
      | _ ->
         AstDP.AAssign (span, transform_lvalue lv, transform_expr e)
     end
  | Ast.AInitialAssign (name, ty, e) ->
     AstDP.AInitialAssign (name, ty, transform_expr e)
  | Ast.AIf (span, e, s1, s2) ->
     AstDP.AIf (span, transform_expr e, transform_stmt s1, transform_stmt s2)
  | Ast.AWhen (span, e, s) ->
     AstDP.AWhen (span, transform_expr e, transform_stmt s)
  | Ast.ACase (span, e, awl) ->
     AstDP.ACase (span, transform_expr e, List.map transform_abstract_when awl)
  | Ast.AWhile (span, e, s) ->
     AstDP.AWhile (span, transform_expr e, transform_stmt s)
  | Ast.AFor { span; name; initial; final; body } ->
     AstDP.AFor { span; name; initial=transform_expr initial; final=transform_expr final; body=transform_stmt body}
  | Ast.ABorrow {span; original; rename; region; body; mode} ->
     AstDP.ABorrow {span; original; rename; region; body=transform_stmt body; mode}
  | Ast.ABlock (span, s1, s2) ->
     AstDP.ABlock (span, transform_stmt s1, transform_stmt s2)
  | Ast.ADiscarding (span, e) ->
     AstDP.ADiscarding (span, transform_expr e)
  | Ast.AReturn (span, e) ->
     AstDP.AReturn (span, transform_expr e)

and transform_abstract_when aw =
  match aw with
  | Ast.AbstractWhen (id, qbl, s) ->
     AstDP.AbstractWhen (id, qbl, transform_stmt s)

and transform_arglist al =
  match al with
  | Ast.Positional el ->
     AstDP.Positional (List.map transform_expr el)
  | Ast.Named idel ->
     AstDP.Named (List.map (fun (id, e) -> (id, transform_expr e)) idel)
