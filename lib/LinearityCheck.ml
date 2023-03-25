(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Tast
open Type
open TypeSystem
open Reporter
open Error

(* Data structures *)

type loop_depth = int
[@@deriving show]

type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed
[@@deriving show]

type state_tbl = (identifier * loop_depth * var_state) list
[@@deriving show]

let empty_tbl: state_tbl = []

let get_entry (tbl: state_tbl) (name: identifier): (loop_depth * var_state) option =
  match (List.find_opt (fun (n, _,_) -> equal_identifier name n) tbl) with
  | Some (_, depth, state) ->
     Some (depth, state)
  | None ->
     None

let get_entry_or_fail (tbl: state_tbl) (name: identifier): (loop_depth * var_state) =
  match get_entry tbl name with
  | Some p -> p
  | None ->
     internal_err ("variable `"
          ^ (ident_string name)
          ^ "` not in state table. Table contents: \n\n"
          ^ (show_state_tbl tbl))

let add_entry (tbl: state_tbl) (name: identifier) (depth: loop_depth): state_tbl =
  match get_entry tbl name with
  | None ->
     (name, depth, Unconsumed) :: tbl
  | Some _ ->
     (* The justification for this being an internal error is that the compiler
        should already have caught a duplicate variable. *)
     internal_err "An entry exists in the state table with this name."

let update_tbl (tbl: state_tbl) (name: identifier) (state: var_state): state_tbl =
  match get_entry tbl name with
  | None ->
     (* The justification for this being an internal error is the compiler
        should have caught a use of a variable that doesn't exist. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (depth, _) ->
     let other_entries = List.filter (fun (n, _,_) -> not (equal_identifier name n)) tbl
     in
     (name, depth, state) :: other_entries

let remove_entry (tbl: state_tbl) (name: identifier): state_tbl =
  match get_entry tbl name with
  | None ->
     (* Internal because it should have been caught by the compiler. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (_, state) ->
     if state = Consumed then
       let others = List.filter (fun (n, _,_) -> not (equal_identifier name n)) tbl
       in
       others
     else
       austral_raise LinearityError [
           Text "Forgot to consume a linear variable: ";
           Code (ident_string name);
           Text "."
         ]

let rec remove_entries (tbl: state_tbl) (names: identifier list): state_tbl =
  match names with
  | first::rest ->
     remove_entries (remove_entry tbl first) rest
  | [] ->
     tbl

let tbl_to_list (tbl: state_tbl): (identifier * loop_depth * var_state) list =
  tbl

type appearances = {
    consumed: int;
    read: int;
    write: int;
    path: int;
  }

let zero_appearances: appearances = {
    consumed = 0;
    read = 0;
    write = 0;
    path = 0;
  }

let consumed_once: appearances = {
    consumed = 1;
    read = 0;
    write = 0;
    path = 0;
  }

let read_once: appearances = {
    consumed = 0;
    read = 1;
    write = 0;
    path = 0;
  }

let write_once: appearances = {
    consumed = 0;
    read = 0;
    write = 1;
    path = 0;
  }

let path_once: appearances = {
    consumed = 0;
    read = 0;
    write = 0;
    path = 1;
  }

let merge (a: appearances) (b: appearances): appearances =
  {
    consumed = a.consumed + b.consumed;
    read = a.read + b.read;
    write = a.write + b.write;
    path = a.path + b.path;
  }

let merge_list (l: appearances list): appearances =
  List.fold_left merge zero_appearances l

(* Counting appearances of variables in expressions *)

let rec count (name: identifier) (expr: texpr): appearances =
  let c = count name in
  match expr with
  | TNilConstant ->
     zero_appearances
  | TBoolConstant _ ->
     zero_appearances
  | TIntConstant _ ->
     zero_appearances
  | TFloatConstant _ ->
     zero_appearances
  | TStringConstant _ ->
     zero_appearances
  | TConstVar _ ->
     (* Constants variables can't be linear. *)
     zero_appearances
  | TParamVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TLocalVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TFunVar _ ->
     zero_appearances
  | TFuncall (_, _, args, _, _) ->
     merge_list (List.map c args)
  | TMethodCall (_, _, _, args, _, _) ->
     merge_list (List.map c args)
  | TVarMethodCall { args; _ } ->
     merge_list (List.map c args)
  | TFptrCall (_, args, _) ->
     merge_list (List.map c args)
  | TCast (e, _) ->
     c e
  | TComparison (_, lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TConjunction (lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TDisjunction (lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TNegation e ->
     c e
  | TIfExpression (e, t, f) ->
     merge (c e) (merge (c t) (c f))
  | TRecordConstructor (_, args) ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TUnionConstructor (_, _, args) ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TSlotAccess (head, _, _) ->
     let head_apps: appearances =
       (* If the head of the path is a variable, check if it is the one we are
          looking for. If it is, count that as a path appearance. *)
       (match head with
        | TParamVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | TLocalVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | _ ->
           (* Otherwise, just count the appearances inside the expression. *)
           c head)
     in
     head_apps
  | TPointerSlotAccess (head, _, _) ->
     let head_apps: appearances =
       (* If the head of the path is a variable, check if it is the one we are
          looking for. If it is, count that as a path appearance. *)
       (match head with
        | TParamVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | TLocalVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | _ ->
           (* Otherwise, just count the appearances inside the expression. *)
           c head)
     in
     head_apps
  | TArrayAccess (head, arr, _) ->
     merge (c head) (c arr)
  | TEmbed (_, _, args) ->
     merge_list (List.map c args)
  | TDeref e ->
     c e
  | TSizeOf _ ->
     zero_appearances
  | TBorrowExpr (mode, name', _, _) ->
     if equal_identifier name name' then
       (match mode with
        | ReadBorrow ->
           read_once
        | WriteBorrow ->
           write_once)
     else
       zero_appearances

(* Utilities *)

let get_state (tbl: state_tbl) (name: identifier): var_state =
  let (_, state) = get_entry_or_fail tbl name in
  state

let get_loop_depth (tbl: state_tbl) (name: identifier): loop_depth =
  let (depth, _) = get_entry_or_fail tbl name in
  depth

let is_unconsumed (tbl: state_tbl) (name: identifier): bool =
  let state = get_state tbl name in
  match state with
  | Unconsumed -> true
  | _ -> false

let universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | _ -> false

let humanize_state (state: var_state): string =
  match state with
  | Unconsumed -> "not yet consumed"
  | BorrowedRead -> "borrowed (read-only)"
  | BorrowedWrite -> "borrowed (read-write)"
  | Consumed -> "consumed"

type partitions =
  | Zero
  | One
  | MoreThanOne

let partition (n: int): partitions =
  if n > 1 then
    MoreThanOne
  else
    if n = 1 then
      One
    else
      if n = 0 then
        Zero
      else
        internal_err "Impossible"

(* Table consistency *)

let tables_are_consistent (stmt_name: string) (a: state_tbl) (b: state_tbl): unit =
  (* Tables should have the same set of variable names. *)
  let names_a: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list a)
  and names_b: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list b)
  in
  if List.equal equal_identifier names_a names_b then
    (* Make a list of triples with the variable name, its state in A, and its
       state in B. *)
    let common: (identifier * var_state * var_state) list =
      List.filter_map (fun (name, _, state_a) ->
          match get_entry b name with
          | Some (_, state_b) ->
             Some (name, state_a, state_b)
          | None ->
             None)
        (tbl_to_list a)
    in
    (* Ensure the states are the same. *)
    List.iter (fun (name, state_a, state_b) ->
        if state_a <> state_b then
          austral_raise LinearityError [
              Text "The variable ";
              Code (ident_string name);
              Text " is used inconsistently in the branches of ";
              Text stmt_name;
              Text " statement. In one branch it is ";
              Text (humanize_state state_a);
              Text " while in the other it is ";
              Text (humanize_state state_b);
              Text "."
            ]
        else
          ()) common
  else
    (* I *think* this is an internal error. *)
    internal_err ("Consumption state tables are inconsistent. This is likely a bug in the linearity checker. Table contents:\n\n"
                  ^ (show_state_tbl a)
                  ^ "\n\n"
                  ^ (show_state_tbl b))

let rec table_list_is_consistent (lst: state_tbl list): unit =
  match lst with
  | a::b::rest ->
     let _ = tables_are_consistent "a case" a b in
     table_list_is_consistent rest
  | [a] ->
     let _ = a in
     ()
  | [] ->
     ()

(* Linearity checking in expressions *)

let rec check_var_in_expr (tbl: state_tbl) (depth: loop_depth) (name: identifier) (expr: texpr): state_tbl =
  (* Count the appearances of the variable in the expression. *)
  let apps: appearances = count name expr in
  (* Destructure apps. *)
  let { consumed: int; write: int; read: int; path: int } = apps in
  (* What is the current state of this variable? *)
  let state: var_state = get_state tbl name in
  (* Make a tuple with the variable's state, and the partitioned appearances. *)
  let tup = (state, partition consumed, partition write, partition read, partition path) in
  match tup with
  (*       State        Consumed      WBorrow       RBorrow      Path    *)
  (* ---------------|-------------|-------------|------------|---------- *)
  | (     Unconsumed,         Zero,         Zero,           _,           _) -> (* Not yet consumed, and at most used through immutable borrows or path reads. *)
     tbl
  | (     Unconsumed,         Zero,          One,        Zero,        Zero) -> (* Not yet consumed, borrowed mutably once, and nothing else. *)
     tbl
  | (     Unconsumed,         Zero,          One,           _,           _) -> (* Not yet consumed, borrowed mutably, then either borrowed immutably or accessed through a path. *)
     error_borrowed_mutably_and_used name
  | (     Unconsumed,         Zero,  MoreThanOne,           _,           _) -> (* Not yet consumed, borrowed mutably more than once. *)
     error_borrowed_mutably_more_than_once name
  | (     Unconsumed,          One,         Zero,        Zero,        Zero) -> (* Not yet consumed, consumed once, and nothing else. Valid IF the loop depth matches. *)
     consume_once tbl depth name
  | (     Unconsumed,          One,            _,           _,           _) -> (* Not yet consumed, consumed once, then either borrowed or accessed through a path. *)
     error_consumed_and_something_else name
  | (     Unconsumed,  MoreThanOne,            _,           _,           _) -> (* Not yet consumed, consumed more than once. *)
     error_consumed_more_than_once name
  | (   BorrowedRead,         Zero,         Zero,        Zero,           _) -> (* Read borrowed, and at most accessed through a path. *)
     tbl
  | (   BorrowedRead,            _,            _,           _,           _) -> (* Read borrowed, and either consumed or borrowed again. *)
     error_read_borrowed_and_something_else name
  | (  BorrowedWrite,         Zero,         Zero,        Zero,        Zero) -> (* Write borrowed, unused. *)
     tbl
  | (  BorrowedWrite,            _,            _,           _,           _) -> (* Write borrowed, used in some way. *)
     error_write_borrowed_and_something_else name
  | (       Consumed,         Zero,         Zero,        Zero,        Zero) -> (* Already consumed, and unused. *)
     tbl
  | (       Consumed,            _,            _,           _,           _) -> (* Already consumed, and used in some way. *)
     error_already_consumed name

and consume_once (tbl: state_tbl) (depth: loop_depth) (name: identifier): state_tbl =
   if depth = get_loop_depth tbl name then
      update_tbl tbl name Consumed
   else
      austral_raise LinearityError [
         Text "The variable ";
         Code (ident_string name);
         Text " was defined outside a loop, but you're trying to consume it inside a loop.";
         Break;
         Text "This is not allowed because it could be consumed zero times or more than once."
       ]

and error_borrowed_mutably_and_used (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " is borrowed mutably, while also being either borrowed or used through a path.";
      Break;
      Text "Mutable borrows cannot appear in the same expression where the variable is used elsewhere."
   ]

and error_borrowed_mutably_more_than_once (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Code " is borrowed mutably multiple times in the same expression."
   ]

and error_consumed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Code " is consumed in the same expression where it is used in some other way.";
      Break;
      Text "A linear variable cannot appear multiple times in the expression that consumes it.";
   ]

and error_consumed_more_than_once (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " is consumed multiple times within the same expression.";
   ]

and error_read_borrowed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " cannot be consumed or borrowed again while it is borrowed (immutably).";
   ]

and error_write_borrowed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text "cannot be used in any way while it is vorrowed (mutably).";
   ]

and error_already_consumed (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " has already been consumed.";
   ]

let check_expr (tbl: state_tbl) (depth: loop_depth) (expr: texpr): state_tbl =
  (* For each variable in the table, check if the variable is used correctly in
     the expression. *)
  let names: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list tbl) in
  let f (tbl: state_tbl) (name: identifier): state_tbl =
    check_var_in_expr tbl depth name expr
  in
  Util.iter_with_context f tbl names

(* Linearity checking *)

let rec check_stmt (tbl: state_tbl) (depth: loop_depth) (stmt: tstmt): state_tbl =
  match stmt with
  | TSkip _ ->
     tbl
  | TLet (_, name, ty, expr, body) ->
     (* First, check the expression. *)
     let tbl: state_tbl = check_expr tbl depth expr in
     (* If the type is linear, add an entry to the table. *)
     if universe_linear_ish (type_universe ty) then
       let tbl: state_tbl = add_entry tbl name depth in
       let tbl: state_tbl = check_stmt tbl depth body in
       (* Once we leave the scope, remove the variable we added. *)
       let tbl: state_tbl = remove_entry tbl name in
       tbl
     else
       check_stmt tbl depth body
  | TDestructure (_, bindings, expr, body) ->
     (* First, check the expression. *)
     let tbl: state_tbl = check_expr tbl depth expr in
     (* Iterate over the bidings, for each that is linear, add an entry to the
        table. Also, keep track of the names of the linear variables. *)
     let linear_names: identifier list =
       List.filter_map (fun (TypedBinding { rename; ty; _ }) ->
           if universe_linear_ish (type_universe ty) then
             Some rename
           else
             None)
         bindings
     in
     let tbl: state_tbl =
       Util.iter_with_context
         (fun tbl (TypedBinding { rename; ty; _ }) ->
           if universe_linear_ish (type_universe ty) then
             add_entry tbl rename depth
           else
             tbl)
         tbl
         bindings
     in
     let tbl: state_tbl = check_stmt tbl depth body in
     (* Once we leave the scope, remove the linear variables we added. *)
     let tbl: state_tbl = remove_entries tbl linear_names in
     tbl
  | TAssign (_, _, expr) ->
     (* Linear values can't be consumed in an L-value, because the only place
        where they could appear is as the index value to an array indexing
        operator (e.g. `[foo(x)]`) and that wouldn't typecheck in the first
        place. *)
     let tbl: state_tbl = check_expr tbl depth expr in
     tbl
  | TIf (_, cond, tb, fb) ->
     let tbl: state_tbl = check_expr tbl depth cond in
     let true_tbl: state_tbl = check_stmt tbl depth tb in
     let false_tbl: state_tbl = check_stmt tbl depth fb in
     let _ = tables_are_consistent "an if" true_tbl false_tbl in
     true_tbl
  | TCase (_, expr, whens, _) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     let tbls: state_tbl list = check_whens tbl depth whens in
     let _ = table_list_is_consistent tbls in
     (match tbls with
      | first::rest ->
         let _ = rest in
         first
      | [] ->
         tbl)
  | TWhile (_, cond, body) ->
     let tbl: state_tbl = check_expr tbl depth cond in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
  | TFor (_, _, start, final, body) ->
     let tbl: state_tbl = check_expr tbl depth start in
     let tbl: state_tbl = check_expr tbl depth final in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
  | TBorrow { original; mode; body; _ } ->
     (* Ensure the original variable is unconsumed to be borrowed. *)
     if is_unconsumed tbl original then
       let tbl: state_tbl =
         match mode with
         | ReadBorrow ->
            update_tbl tbl original BorrowedRead
         | WriteBorrow ->
            update_tbl tbl original BorrowedWrite
       in
       (* Traverse the body. *)
       let tbl: state_tbl = check_stmt tbl depth body in
       (* After the body, unborrow the variable. *)
       let tbl: state_tbl = update_tbl tbl original Unconsumed in
       tbl
     else
       let state: var_state = get_state tbl original in
       austral_raise LinearityError [
           Text "Cannot borrow the variable ";
           Code (ident_string original);
           Text " because it is already ";
           Text (humanize_state state);
           Text "."
         ]
  | TBlock (_, a, b) ->
     let tbl: state_tbl = check_stmt tbl depth a in
     let tbl: state_tbl = check_stmt tbl depth b in
     tbl
  | TDiscarding (_, expr) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     tbl
  | TReturn (_, expr) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     (* Ensure that all variables are Consumed. *)
     let _ =
       List.map (fun (name, _, state) ->
           if state = Consumed then
             ()
           else
             austral_raise LinearityError [
                 Text "The variable ";
                 Code (ident_string name);
                 Text " is not consumed by the time of the return statement. Did you forget to call a destructure, or destructure the contents?"
               ])
         (tbl_to_list tbl)
     in
     tbl

and check_whens (tbl: state_tbl) (depth: loop_depth) (whens: typed_when list): state_tbl list =
  List.map (check_when tbl depth) whens

and check_when (tbl: state_tbl) (depth: loop_depth) (whn: typed_when): state_tbl =
  let TypedWhen (_, bindings, body) = whn in
  (* Iterate over the bidings, for each that is linear, add an entry to the
     table. Keep track of the names of the linear variables we added. *)
  let linear_names: identifier list =
    List.filter_map (fun (TypedBinding { ty; rename; _ }) ->
        if universe_linear_ish (type_universe ty) then
          Some rename
        else
          None)
      bindings
  in
  let tbl: state_tbl =
    Util.iter_with_context
      (fun tbl (TypedBinding { rename; ty; _ }) ->
        if universe_linear_ish (type_universe ty) then
          add_entry tbl rename depth
        else
          tbl)
      tbl
      bindings
  in
  (* Check the body. *)
  let tbl: state_tbl = check_stmt tbl depth body in
  (* Once we leave the scope, remove the linear variables we added. *)
  let tbl: state_tbl = remove_entries tbl linear_names in
  tbl

let init_tbl (tbl: state_tbl) (params: value_parameter list): state_tbl =
  let f (tbl: state_tbl) (ValueParameter (name, ty)): state_tbl =
    if universe_linear_ish (type_universe ty) then
      (* Add this parameter to the list at depth zero. *)
      add_entry tbl name 0
    else
      tbl
  in
  Util.iter_with_context f tbl params

let linearity_check (params: value_parameter list) (body: tstmt): unit =
  (* Initialize the loop depth to zero. *)
  let depth: int = 0 in
  (* Initialize the state table to the empty table. *)
  let tbl: state_tbl = empty_tbl in
  (* Populate the table with the linear parameters. *)
  let tbl: state_tbl = init_tbl tbl params in
  (* Traverse the code in execution order. *)
  let _ = check_stmt tbl depth body in
  ()

(* Linearity checking of whole modules *)

let rec check_module_linearity (TypedModule (_, decls)): unit =
  with_frame "Linearity Checker"
    (fun _ ->
      let _ = List.map check_decl_linearity decls in
      ())

and check_decl_linearity (decl: typed_decl): unit =
  match decl with
  | TFunction (_, _, name, _, params, _, b, _) ->
     with_frame ("Checking linearity of function " ^ (ident_string name))
       (fun _ -> linearity_check params b)
  | TInstance (_, _, _, _, _, methods, _) ->
     let _ = List.map check_method_linearity methods in
     ()
  | _ ->
     ()

and check_method_linearity (TypedMethodDef (_, name, params, _, b)) =
  with_frame ("Checking linearity of method " ^ (ident_string name))
    (fun _ -> linearity_check params b)
