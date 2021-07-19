open Imports
open Cst
open Ast
open Qualifier
open Error

let rec abs_stmt im stmt =
  match stmt with
  | CSkip ->
     ASkip
  | CLet _ ->
     err "Let statement not in a list context"
  | CDestructure _ ->
     err "Destructure statement not in a list context"
  | CAssign (name, value) ->
     AAssign (name, abs_expr im value)
  | CIf (c, t, f) ->
     AIf (abs_expr im c, abs_stmt im t, abs_stmt im f)
  | CCase (e, cases) ->
     ACase (abs_expr im e, List.map (abs_when im) cases)
  | CWhile (c, b) ->
     AWhile (abs_expr im c, abs_stmt im b)
  | CFor (n, i, f, b) ->
     AFor {
         name = n;
         initial = abs_expr im i;
         final = abs_expr im f;
         body = abs_stmt im b
       }
  | CBorrow { original; rename; region; body } ->
     ABorrow { original; rename; region; body=abs_stmt im body }
  | CBlock l ->
     let_reshape im l
  | CDiscarding e ->
     ADiscarding (abs_expr im e)
  | CReturn e ->
     AReturn (abs_expr im e)

and abs_expr im expr =
  match expr with
  | CNilConstant -> NilConstant
  | CBoolConstant b -> BoolConstant b
  | CIntConstant i -> IntConstant i
  | CFloatConstant f -> FloatConstant f
  | CStringConstant s -> StringConstant s
  | CVariable i -> Variable i
  | CFuncall (name, args) ->
     FunctionCall (qualify_identifier im name, abs_arglist im args)
  | CArith (op, lhs, rhs) ->
     ArithmeticExpression (op, abs_expr im lhs, abs_expr im rhs)
  | CComparison (op, lhs, rhs) ->
     Comparison (op, abs_expr im lhs, abs_expr im rhs)
  | CConjunction (lhs, rhs) ->
     Conjunction (abs_expr im lhs, abs_expr im rhs)
  | CDisjunction (lhs, rhs) ->
     Disjunction (abs_expr im lhs, abs_expr im rhs)
  | CNegation e ->
     Negation (abs_expr im e)
  | CIfExpression (c, t, f) ->
     IfExpression (abs_expr im c, abs_expr im t, abs_expr im f)
  | CPath (e, es) ->
     Path (abs_expr im e, List.map abs_path_elem es)

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

and abs_path_elem elem =
  match elem with
  | CSlotAccessor i ->
     SlotAccessor i
  | CPointerSlotAccessor i ->
     PointerSlotAccessor i

(* Given a list of statements, find the first let statement, if any, and put the
   remainder of the list under its body. Then call let_reshape on that
   remainder. *)
and let_reshape (im: import_map) (l: cstmt list): astmt =
  match l with
  | first::rest ->
     (match first with
      | CLet (n, t, v) ->
         ALet (n, qualify_typespec im t, abs_expr im v, let_reshape im rest)
      | CDestructure (bs, e) ->
         let bs' = List.map (fun (n, ts) -> (n, qualify_typespec im ts)) bs
         and e' = abs_expr im e
         and b = let_reshape im rest
         in
         ADestructure (bs', e', b)
      | s ->
         ABlock (abs_stmt im s, let_reshape im rest))
  | [] ->
     ASkip
