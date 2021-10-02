open Imports
open Cst
open Ast
open Escape
open Qualifier
open Error

let rec abs_stmt im stmt =
  match stmt with
  | CSkip _ ->
     ASkip
  | CLet _ ->
     err "Let statement not in a list context"
  | CDestructure _ ->
     err "Destructure statement not in a list context"
  | CAssign (_, lvalue, value) ->
     AAssign (abs_lvalue im lvalue, abs_expr im value)
  | CIf (_, c, t, f) ->
     AIf (abs_expr im c, abs_stmt im t, abs_stmt im f)
  | CCase (_, e, cases) ->
     ACase (abs_expr im e, List.map (abs_when im) cases)
  | CWhile (_, c, b) ->
     AWhile (abs_expr im c, abs_stmt im b)
  | CFor (_, n, i, f, b) ->
     AFor {
         name = n;
         initial = abs_expr im i;
         final = abs_expr im f;
         body = abs_stmt im b
       }
  | CBorrow { original; rename; region; body; mode; _ } ->
     ABorrow { original; rename; region; body=abs_stmt im body; mode }
  | CBlock (_, l) ->
     let_reshape im l
  | CDiscarding (_, e) ->
     ADiscarding (abs_expr im e)
  | CReturn (_, e) ->
     AReturn (abs_expr im e)

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
     ArithmeticExpression (op, abs_expr im lhs, abs_expr im rhs)
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
  | CPathRef (_, e, es) ->
     PathRef (abs_expr im e, List.map (abs_path_elem im) es)

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
      | CLet (_, n, t, v) ->
         ALet (n, qualify_typespec im t, abs_expr im v, let_reshape im rest)
      | CDestructure (_, bs, e) ->
         let bs' = List.map (fun (n, ts) -> (n, qualify_typespec im ts)) bs
         and e' = abs_expr im e
         and b = let_reshape im rest
         in
         ADestructure (bs', e', b)
      | s ->
         ABlock (abs_stmt im s, let_reshape im rest))
  | [] ->
     ASkip

and abs_lvalue im (ConcreteLValue (head, elems)) =
  LValue (head, List.map (abs_path_elem im) elems)
