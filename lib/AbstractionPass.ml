open Imports
open Identifier
open BuiltIn
open Cst
open Ast
open Escape
open Qualifier
open Span
open Error

let rec abs_stmt im stmt =
  match stmt with
  | CSkip span ->
     ASkip span
  | CLet _ ->
     err "Let statement not in a list context"
  | CDestructure _ ->
     err "Destructure statement not in a list context"
  | CAssign (span, lvalue, value) ->
     AAssign (span, abs_lvalue im lvalue, abs_expr im value)
  | CIf (span, c, t, f) ->
     AIf (span, abs_expr im c, abs_stmt im t, abs_stmt im f)
  | CCase (span, e, cases) ->
     ACase (span, abs_expr im e, List.map (abs_when im) cases)
  | CWhile (span, c, b) ->
     AWhile (span, abs_expr im c, abs_stmt im b)
  | CFor (span, n, i, f, b) ->
     AFor {
         span = span;
         name = n;
         initial = abs_expr im i;
         final = abs_expr im f;
         body = abs_stmt im b
       }
  | CBorrow { span; original; rename; region; body; mode; _ } ->
     ABorrow { span; original; rename; region; body=abs_stmt im body; mode }
  | CBlock (_, l) ->
     let_reshape im l
  | CDiscarding (span, e) ->
     ADiscarding (span, abs_expr im e)
  | CReturn (span, e) ->
     AReturn (span, abs_expr im e)

and abs_expr im expr =
  match expr with
  | CNilConstant _ -> NilConstant
  | CBoolConstant (_, b) -> BoolConstant b
  | CIntConstant (_, i) -> IntConstant i
  | CFloatConstant (_, f) -> FloatConstant f
  | CStringConstant (_, s) -> StringConstant (escape_string s)
  | CVariable (_, i) -> Variable (qualify_identifier im i)
  | CFuncall (_, name, args) ->
     FunctionCall (qualify_identifier im name, abs_arglist im args)
  | CArith (_, op, lhs, rhs) ->
     let op_name =
       match op with
       | Add ->
          "Trapping_Add"
       | Subtract ->
          "Trapping_Subtract"
       | Multiply ->
          "Trapping_Multiply"
       | Divide ->
          "Trapping_Divide"
     in
     let op_qname = make_qident (pervasive_module_name, make_ident op_name, make_ident op_name) in
     FunctionCall (op_qname, Positional [abs_expr im lhs; abs_expr im rhs])
  | CComparison (_, op, lhs, rhs) ->
     Comparison (op, abs_expr im lhs, abs_expr im rhs)
  | CConjunction (_, lhs, rhs) ->
     Conjunction (abs_expr im lhs, abs_expr im rhs)
  | CDisjunction (_, lhs, rhs) ->
     Disjunction (abs_expr im lhs, abs_expr im rhs)
  | CNegation (_, e) ->
     Negation (abs_expr im e)
  | CIfExpression (_, c, t, f) ->
     IfExpression (abs_expr im c, abs_expr im t, abs_expr im f)
  | CPath (_, e, es) ->
     Path (abs_expr im e, List.map (abs_path_elem im) es)
  | CEmbed (_, ty, expr, args) ->
     Embed (qualify_typespec im ty, expr, List.map (abs_expr im) args)

and abs_when im (ConcreteWhen (name, params, body)) =
  AbstractWhen (name,
                List.map (fun (ConcreteParam (n, t)) -> (n, qualify_typespec im t)) params,
                abs_stmt im body)

and abs_arglist im args =
  match args with
  | ConcretePositionalArgs l ->
     Positional (List.map (abs_expr im) l)
  | ConcreteNamedArgs l ->
     Named (List.map (fun (n, v) -> (n, abs_expr im v)) l)

and abs_path_elem im elem =
  match elem with
  | CSlotAccessor i ->
     SlotAccessor i
  | CPointerSlotAccessor i ->
     PointerSlotAccessor i
  | CArrayIndex ie ->
     ArrayIndex (abs_expr im ie)

(* Given a list of statements, find the first let statement, if any, and put the
   remainder of the list under its body. Then call let_reshape on that
   remainder. *)
and let_reshape (im: import_map) (l: cstmt list): astmt =
  match l with
  | first::rest ->
     (match first with
      | CLet (span, n, t, v) ->
         ALet (span, n, qualify_typespec im t, abs_expr im v, let_reshape im rest)
      | CDestructure (span, bs, e) ->
         let bs' = List.map (fun (n, ts) -> (n, qualify_typespec im ts)) bs
         and e' = abs_expr im e
         and b = let_reshape im rest
         in
         ADestructure (span, bs', e', b)
      | s ->
         ABlock (empty_span, abs_stmt im s, let_reshape im rest))
  | [] ->
     ASkip empty_span

and abs_lvalue im (ConcreteLValue (head, elems)) =
  LValue (head, List.map (abs_path_elem im) elems)
