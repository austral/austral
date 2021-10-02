open Identifier
open Type
open TypeSystem
open Tast
open Error

let lerr msg =
  err msg

let type_is_write_ref = function
  | WriteRef _ ->
     true
  | _ ->
     false

(* Count the number of times that a variable with the given name appears in an
   expression. *)
let rec count_appearances (name: identifier) (expr: texpr) =
  let ca = count_appearances name
  and sum l = List.fold_left (+) 0 l
  in
  match expr with
  | TNilConstant ->
     0
  | TBoolConstant _ ->
     0
  | TIntConstant _ ->
     0
  | TFloatConstant _ ->
     0
  | TStringConstant _ ->
     0
  | TVariable (n, _) ->
     (* TODO: is using original_name here correct? *)
     if equal_identifier name (original_name n) then
       1
     else
       0
  | TArithmetic (_, lhs, rhs) ->
     (ca lhs) + (ca rhs)
  | TFuncall (_, args, _, _) ->
     sum (List.map ca args)
  | TMethodCall (_, _, args, _) ->
     sum (List.map ca args)
  | TCast (e, _) ->
     ca e
  | TComparison (_, lhs, rhs) ->
     (ca lhs) + (ca rhs)
  | TConjunction (lhs, rhs) ->
     (ca lhs) + (ca rhs)
  | TDisjunction (lhs, rhs) ->
     (ca lhs) + (ca rhs)
  | TNegation e ->
     ca e
  | TIfExpression (c, t, f) ->
     (ca c) + (ca t) + (ca f)
  | TRecordConstructor (_, args) ->
     sum (List.map (fun (_, e) -> ca e) args)
  | TUnionConstructor (_, _, args) ->
     sum (List.map (fun (_, e) -> ca e) args)
  | TPath (e, elems) ->
     (* All paths end in a free value. If the head of the path is the variable
        we're counting, we don't count this as an appearance. This is for
        programmer ergonomics. It's also so that if we have a record `r` with
        two linear values `a` and `b`, an expression like `r.a` won't consume
        `r` and leave `r.b` dangling.

        And this applies inductively to all other struct accessors: if we have
        `x.y.z.w` it doesn't matter if `x` is a linear variable and `y` and `z`
        are values of a linear type, as long as `w` is free. *)
     let e' =
       (* Count the number of appearances of this variable in the expression,
          unless the expression is just a bare variable. So `f(x).y` will count
          but not `x.y`. *)
       (match e with
        | TVariable _ ->
           0
        | _ ->
           ca e)
     in
     let ca_path elem =
       (match elem with
        | TSlotAccessor _ ->
           0
        | TPointerSlotAccessor _ ->
           0
        | TArrayIndex (ie, _) ->
           count_appearances name ie)
     in
     e' + (sum (List.map ca_path elems))
  | TPathRef (e, elems, _, _) ->
     let e' =
       (match e with
        | TVariable _ ->
           0
        | _ ->
           ca e)
     in
     let ca_path elem =
       (match elem with
        | TSlotAccessor _ ->
           0
        | TPointerSlotAccessor _ ->
           0
        | TArrayIndex (ie, _) ->
           count_appearances name ie)
     in
     e' + (sum (List.map ca_path elems))

(* Represents the state of a linear variable *)
type state =
  | Unconsumed
  | Consumed

(* Given the name of a linear variable, an expression, and the current state of
   that variable, calculate the next state it's in.

  If the variable is unconsumed, check it either never appears (continues to be
   unconsumed) or appears once (now consumed). Error if it appears more than
   once.

  IF the variable is already consumed, check it is not consumed again. Its state
   continues to be consumed. *)
let new_state (name: identifier) (expr: texpr) (state: state): state =
  let a = count_appearances name expr in
  match state with
  | Unconsumed ->
     if a = 0 then
       Unconsumed
     else
       if a = 1 then
         Consumed
       else
         lerr "Linear variable used more than once in the same expression."
  | Consumed ->
     if a = 0 then
       Consumed
     else
       lerr ("Linear variable consumed again: " ^ (ident_string name))

let rec check_consistency (name: identifier) (is_write_ref: bool) (stmt: tstmt): unit =
  let state = check_consistency' name is_write_ref stmt Unconsumed in
  if state = Consumed then
    ()
  else
    if is_write_ref then
      ()
    else
      lerr "Linear variable not consumed."

(* Check that a linear variable with the given name is used consistently in the
   body in which it is defined.

   The state parameter tells us the current state of the variable: whether it is
   unconsumed or already consumed.

   Returns the state the variable is in after the body: either consumed or
   unconsumed. If the variable is used incorrectly, raises an error. *)
and check_consistency' (name: identifier) (is_write_ref: bool) (stmt: tstmt) (state: state): state =
  match stmt with
  | TSkip ->
     state
  | TLet (_, _, e, b) ->
     let state' = new_state name e state in
     check_consistency' name is_write_ref b state'
  | TDestructure (_, e, b) ->
    let state' = new_state name e state in
    check_consistency' name is_write_ref b state'
  | TAssign (_, e) ->
     new_state name e state
  | TIf (c, tb, fb) ->
     let state' = new_state name c state in
     let true_branch_state = check_consistency' name is_write_ref tb state'
     and false_branch_state = check_consistency' name is_write_ref fb state' in
     (* In an if statement, the state of the variable must be the same in both
        branches. *)
     if true_branch_state = false_branch_state then
       true_branch_state
     else
       lerr "Linear variable is consumed in one branch but not others."
  | TCase (e, whens) ->
     let state' = new_state name e state in
     let when_states = List.map (fun (TypedWhen (_, _, b)) -> check_consistency' name is_write_ref b state') whens in
     (* In a case statement, the state must be the same in all branches *)
     if same_state when_states then
       List.nth when_states 0
     else
       lerr "Linear variable is consumed in one branch but not others."
  | TWhile (e, b) ->
     (* Linear variables cannot appear in either the condition or body of a loop *)
     let ea = count_appearances name e
     and bs = check_consistency' name is_write_ref b state in
     if ea > 0 then
       lerr "Linear variables cannot appear in the condition of a while loop."
     else
       if (state = Unconsumed) && (bs = Consumed) then
         lerr "Linear variables cannot appear in the body of a while loop."
       else
         state
  | TFor (_, i, f, b) ->
     let state' = new_state name i state in
     let state'' = new_state name f state' in
     let bs = check_consistency' name is_write_ref b state'' in
     if (state = Unconsumed) && (bs = Consumed) then
       lerr "Linear variables cannot appear in the body of a for loop."
     else
       state''
  | TBorrow { original; body; _ } ->
     (* We don't count borrow statements as consuming a variable. However, if
        this borrow refers to this variable, then the variable cannot appear in
        the body of the borrow. *)
     if equal_identifier name original then
       let bs = check_consistency' name is_write_ref  body state in
       if (state = Unconsumed) && (bs = Consumed) then
         lerr "Linear variables cannot appear in the scope within which they are borrowed."
       else
         bs
     else
       check_consistency' name is_write_ref body state
  | TBlock (a, b) ->
      let state' = check_consistency' name is_write_ref a state in
      check_consistency' name is_write_ref b state'
  | TDiscarding e ->
     new_state name e state
  | TReturn e ->
     let state' = new_state name e state in
     if state' = Unconsumed then
       if is_write_ref then
         Unconsumed
       else
         lerr ("Can't return from a function while there are unconsumed linear values. Name: "
               ^ (ident_string name))
     else
       state'

(* Check whether all states in the list are the same state *)
and same_state (states: state list): bool =
  assert ((List.length states) > 0);
  let is_un = List.mem Unconsumed states
  and is_c = List.mem Consumed states in
  xor is_un is_c

and xor (a: bool) (b: bool): bool =
  (a || b) || (not (a && b))

let universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | _ -> false

(* Check that all linear variables defined in this statement are used
   consistently. *)
let rec check_linearity (stmt: tstmt): unit =
  match stmt with
  | TSkip ->
     ()
  | TLet (n, t, _, b) ->
     let u = type_universe t in
     if universe_linear_ish u then
       check_consistency n (type_is_write_ref t) b
     else
       ()
  | TDestructure (bs, _, b) ->
     let check' (n, t) =
         let u = type_universe t in
         if universe_linear_ish u then
           check_consistency n (type_is_write_ref t) b
         else
           ()
     in
     let _ = List.map check' bs in ()
  | TAssign _ ->
     ()
  | TIf (_, tb, fb) ->
     check_linearity tb;
     check_linearity fb
  | TCase (_, whens) ->
     let _ = List.map (fun (TypedWhen (_, _, b)) -> check_linearity b) whens in
     ()
  | TWhile (_, b) ->
     check_linearity b
  | TFor (_, _, _, b) ->
     check_linearity b
  | TBorrow { body; _ } ->
     check_linearity body
  | TBlock (a, b) ->
     check_linearity a;
     check_linearity b
  | TDiscarding _ ->
     ()
  | TReturn _ ->
     ()

let rec check_decl_linearity (decl: typed_decl): unit =
  match decl with
  | TFunction (_, _, _, params, _, b, _) ->
     (* Check linearity of parameters *)
     let _ = List.map (check_param b) params in
     (* Check linearity within the code *)
     check_linearity b
  | TInstance (_, _, _, _, methods, _) ->
     let _ = List.map check_method_linearity methods in
     ()
  | _ ->
     ()

and check_method_linearity (TypedMethodDef (_, params, _, b)) =
  (* Check linearity of parameters *)
  let _ = List.map (check_param b) params in
  (* Check linearity within the code *)
  check_linearity b

and check_param (b: tstmt) (ValueParameter (n, t)) =
  if universe_linear_ish (type_universe t) then
    check_consistency n (type_is_write_ref t) b
  else
    ()
