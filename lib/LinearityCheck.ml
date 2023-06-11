(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Stages.Tast
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

let universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | _ -> false

type state_tbl = StateTable of (identifier * ty * loop_depth * var_state) list * identifier list
[@@deriving show]

let empty_tbl: state_tbl = StateTable ([], [])

let table_rows (tbl: state_tbl): (identifier * ty * loop_depth * var_state) list =
  let (StateTable (rows, _)) = tbl in
  rows

let get_entry (tbl: state_tbl) (name: identifier): (ty * loop_depth * var_state) option =
  match (List.find_opt (fun (n, _, _,_) -> equal_identifier name n) (table_rows tbl)) with
  | Some (_, ty, depth, state) ->
     Some (ty, depth, state)
  | None ->
     None

let get_entry_or_fail (tbl: state_tbl) (name: identifier): (ty * loop_depth * var_state) =
  match get_entry tbl name with
  | Some p -> p
  | None ->
     internal_err ("variable `"
          ^ (ident_string name)
          ^ "` not in state table. Table contents: \n\n"
          ^ (show_state_tbl tbl))

let add_entry (tbl: state_tbl) (name: identifier) (ty: ty) (depth: loop_depth): state_tbl =
  let (StateTable (rows, pending)) = tbl in
  match get_entry tbl name with
  | None ->
     StateTable ((name, ty, depth, Unconsumed) :: rows, pending)
  | Some _ ->
     (* The justification for this being an internal error is that the compiler
        should already have caught a duplicate variable. *)
     internal_err "An entry exists in the state table with this name."

let trim_free (tbl: state_tbl): state_tbl =
  let (StateTable (entries, pending)) = tbl in
  let trim tuple =
    let (_, ty, _, _) = tuple in
    if universe_linear_ish (type_universe ty) then
      Some tuple
    else
      None
  in
  let entries = List.filter_map trim entries in
  StateTable (entries, pending)

let update_tbl (tbl: state_tbl) (name: identifier) (state: var_state): state_tbl =
  match get_entry tbl name with
  | None ->
     (* The justification for this being an internal error is the compiler
        should have caught a use of a variable that doesn't exist. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (ty, depth, _) ->
     let (StateTable (rows, pending)) = tbl in
     let other_entries = List.filter (fun (n, _, _,_) -> not (equal_identifier name n)) rows
     in
     StateTable ((name, ty, depth, state) :: other_entries, pending)

let remove_entry (tbl: state_tbl) (name: identifier): state_tbl =
  match get_entry tbl name with
  | None ->
     (* Internal because it should have been caught by the compiler. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (ty, _, state) ->
     let is_write_ref: bool = match ty with WriteRef _ -> true | _ -> false in
     if (state = Consumed) || is_write_ref then
       let (StateTable (rows, pending)) = tbl in
       let others = List.filter (fun (n, _, _, _) -> not (equal_identifier name n)) rows in
       StateTable (others, pending)
     else
       if universe_linear_ish (type_universe ty) then
         austral_raise LinearityError [
             Text "Forgot to consume a linear variable: ";
             Code (ident_string name);
             Text "."
           ]
       else
         tbl

let rec remove_entries (tbl: state_tbl) (names: identifier list): state_tbl =
  match names with
  | first::rest ->
     remove_entries (remove_entry tbl first) rest
  | [] ->
     tbl

let tbl_to_list (tbl: state_tbl): (identifier * ty * loop_depth * var_state) list =
  table_rows tbl

let get_pending (tbl: state_tbl): identifier list =
  let (StateTable (_, pending)) = tbl in
  pending

let is_pending (tbl: state_tbl) (name: identifier): bool =
  List.exists (fun elem -> equal_identifier name elem) (get_pending tbl)

let mark_pending (tbl: state_tbl) (name: identifier): state_tbl =
  let (StateTable (rows, pending)) = tbl in
  if is_pending tbl name then
    internal_err "Identifier twice marked pending."
  else
    StateTable (rows, name :: pending)

let remove_pending (tbl: state_tbl) (name: identifier): state_tbl =
  if not (is_pending tbl name) then
    internal_err "Tried to remove_pending an identifier not on the list of pending variables."
  else
    let (StateTable (rows, pending)) = tbl in
    let pending' = List.filter (fun elem -> not (equal_identifier name elem)) pending in
    StateTable (rows, pending')

let subtract (a: identifier list) (b: identifier list): identifier list =
  (* elements that are in a and not in b *)
  let is_in (name: identifier) (l: identifier list): bool =
    List.exists (fun elem -> equal_identifier name elem) l
  in
  List.filter (fun elem -> not (is_in elem b)) a

let pending_error (names: identifier list) =
  let c: int = List.length names in
  austral_raise LinearityError [
      Text "The ";
      if (c = 1) then
        Text " variable "
      else
        Text " variables ";
      Code (String.concat ", " (List.map ident_string names));
      if (c = 1) then
        Text " was "
      else
        Text " were ";
      Text "consumed in the loop, without afterwards being reassigned."
    ]

type appearances = {
    consumed: int;
    path: int;
  }

let zero_appearances: appearances = {
    consumed = 0;
    path = 0;
  }

let consumed_once: appearances = {
    consumed = 1;
    path = 0;
  }

let path_once: appearances = {
    consumed = 0;
    path = 1;
  }

let merge (a: appearances) (b: appearances): appearances =
  {
    consumed = a.consumed + b.consumed;
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
  | TTemporary (name', _) ->
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
  | TPath { head; elems; _ } ->
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
     and path_apps: appearances = merge_list (List.map (count_path_elem name) elems)
     in
     merge head_apps path_apps
  | TRefPath (head, elems, _) ->
     let head_apps: appearances =
       (* If the head of the path is a variable, check if it is the one we are
          looking for. If it is, count that as an appearance, because
          transforming a mutable reference consumes it. *)
       (match head with
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
        | _ ->
           (* Otherwise, just count the appearances inside the expression. *)
           c head)
     and path_apps: appearances = merge_list (List.map count_ref_path_elem elems)
     in
     merge head_apps path_apps
  | TEmbed (_, _, args) ->
     merge_list (List.map c args)
  | TDeref e ->
     c e
  | TSizeOf _ ->
     zero_appearances

and count_path_elem (name: identifier) (elem: typed_path_elem): appearances =
  match elem with
  | TSlotAccessor _ ->
     zero_appearances
  | TPointerSlotAccessor _ ->
     zero_appearances
  | TArrayIndex (e, _) ->
     count name e

and count_ref_path_elem (elem: typed_ref_path_elem): appearances =
  match elem with
  | TRefSlotAccessor _ ->
     zero_appearances

(* Utilities *)

let get_state (tbl: state_tbl) (name: identifier): var_state =
  let (_, _, state) = get_entry_or_fail tbl name in
  state

let get_loop_depth (tbl: state_tbl) (name: identifier): loop_depth =
  let (_, depth, _) = get_entry_or_fail tbl name in
  depth

let is_unconsumed (tbl: state_tbl) (name: identifier): bool =
  let state = get_state tbl name in
  match state with
  | Unconsumed -> true
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
  let a: state_tbl = trim_free a
  and b: state_tbl = trim_free b in
  (* Tables should have the same set of variable names. *)
  let state_table_names (tbl: state_tbl): identifier list =
    let l: identifier list = List.map (fun (name, _, _, _) -> name) (tbl_to_list tbl) in
    List.sort (fun a b -> String.compare (ident_string a) (ident_string b)) l
  in
  let names_a: identifier list = state_table_names a
  and names_b: identifier list = state_table_names b
  in
  if List.equal equal_identifier names_a names_b then
    (* Make a list of triples with the variable name, its state in A, and its
       state in B. *)
    let common: (identifier * var_state * var_state) list =
      List.filter_map (fun (name, _, _, state_a) ->
          match get_entry b name with
          | Some (_, _, state_b) ->
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

let rec check_var_in_expr (tbl: state_tbl) (depth: loop_depth) (name: identifier) (ty: ty) (expr: texpr): state_tbl =
  (* Count the appearances of the variable in the expression. *)
  let apps: appearances = count name expr in
  (* Destructure apps. *)
  let { consumed: int; path: int; } = apps in
  (* What is the current state of this variable? *)
  let state: var_state = get_state tbl name in
  (* Make a tuple with the variable's state, and the partitioned appearances. *)
  let tup = (state, partition consumed, partition path) in
  match tup with
  (*       State        Consumed       Path    *)
  (* ---------------|-------------|----------- *)
  | (     Unconsumed,         Zero,            _) -> (* Not yet consumed, and at most read through a path. *)
     tbl
  | (     Unconsumed,          One,         Zero) -> (* Not yet consumed, consumed once, and nothing else. Valid (but owed if the loop depth doesn't match). *)
     consume_once tbl depth name ty
  | (     Unconsumed,          One,            _) -> (* Not yet consumed, consumed once, accessed through a path. Valid if it's a mutable reference. *)
     let _ = maybe_error_consumed_and_accessed_through_path name ty in tbl
  | (     Unconsumed,  MoreThanOne,            _) -> (* Not yet consumed, consumed more than once. *)
     error_consumed_more_than_once name ty;
     tbl
  | (   BorrowedRead,         Zero,            _) -> (* Read borrowed, and at most accessed through a path. *)
     tbl
  | (   BorrowedRead,            _,            _) -> (* Read borrowed, and either consumed, reborrowed or borrowed again. *)
     error_read_borrowed_and_something_else name
  | (  BorrowedWrite,         Zero,         Zero) -> (* Write borrowed, unused. *)
     tbl
  | (  BorrowedWrite,            _,            _) -> (* Write borrowed, used in some way. *)
     error_write_borrowed_and_something_else name
  | (       Consumed,         Zero,         Zero) -> (* Already consumed, and unused. *)
     tbl
  | (       Consumed,            _,            _) -> (* Already consumed, and used in some way. *)
     error_already_consumed name ty;
     tbl

and consume_once (tbl: state_tbl) (depth: loop_depth) (name: identifier) (ty: ty): state_tbl =
  let tbl: state_tbl = update_tbl tbl name Consumed in
  if depth <> get_loop_depth tbl name then
    (* Consumed inside a loop, so mark it as pending. *)
    if universe_linear_ish (type_universe ty) then
      mark_pending tbl name
    else
      tbl
  else
    (* Nothing else to do. *)
    tbl

and error_consumed_more_than_once (name: identifier) (ty: ty) =
  if universe_linear_ish (type_universe ty) then
    austral_raise LinearityError [
        Text "The variable ";
        Code (ident_string name);
        Text " is consumed multiple times within the same expression.";
      ]
  else
    ()

and maybe_error_consumed_and_accessed_through_path (name: identifier) (ty: ty) =
   match ty with
   | WriteRef _ ->
     ()
   | _ ->
      austral_raise LinearityError [
         Text "The variable ";
         Code (ident_string name);
         Text " cannot be consumed and also accessed through a path in the same expression.";
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

and error_already_consumed (name: identifier) (ty: ty) =
  if universe_linear_ish (type_universe ty) then
    austral_raise LinearityError [
        Text "The variable ";
        Code (ident_string name);
        Text " has already been consumed.";
      ]
  else
    ()

let check_expr (tbl: state_tbl) (depth: loop_depth) (expr: texpr): state_tbl =
  (* For each variable in the table, check if the variable is used correctly in
     the expression. *)
  let vars: (identifier * ty) list = List.map (fun (name, ty, _, _) -> (name, ty)) (tbl_to_list tbl) in
  let f (tbl: state_tbl) (name, ty): state_tbl =
    check_var_in_expr tbl depth name ty expr
  in
  Util.iter_with_context f tbl vars

(* Linearity checking *)

let rec check_stmt (tbl: state_tbl) (depth: loop_depth) (stmt: tstmt): state_tbl =
  match stmt with
  | TSkip _ ->
     tbl
  | TLet (span, _, name, ty, body) ->
     adorn_error_with_span span
       (fun _ ->
         (* Add an entry to the table. *)
         let tbl: state_tbl = add_entry tbl name ty depth in
         let tbl: state_tbl = check_stmt tbl depth body in
         (* Once we leave the scope, remove the variable we added. *)
         let tbl: state_tbl = remove_entry tbl name in
         tbl)
  | TLetTmp (name, ty, expr, body) ->
     (* Add an entry to the table. *)
     let tbl: state_tbl = add_entry tbl name ty depth in
     let tbl: state_tbl = check_expr tbl depth expr in
     let tbl: state_tbl = check_stmt tbl depth body in
     (* Once we leave the scope, remove the variable we added. *)
     let tbl: state_tbl = remove_entry tbl name in
     tbl
  | TAssignTmp (name, expr) ->
    let tbl: state_tbl = check_expr tbl depth expr in
    begin
      match get_entry tbl name with
      | Some (ty, _, state) ->
         begin
           match state with
           | Unconsumed | BorrowedRead | BorrowedWrite ->
              if universe_linear_ish (type_universe ty) then
                austral_raise LinearityError [
                    Text "Cannot assign to the variable ";
                    Code (ident_string name);
                    Text " because it is not yet consumed."
                  ]
              else
                tbl
           | Consumed ->
              let tbl: state_tbl = update_tbl tbl name Unconsumed in
              if is_pending tbl name then
                remove_pending tbl name
              else
                tbl
         end
      | None ->
         tbl
    end
  | TDestructure (span, _, bindings, expr, body) ->
     adorn_error_with_span span
       (fun _ ->
         (* First, check the expression. *)
         let tbl: state_tbl = check_expr tbl depth expr in
         (* Iterate over the bidings, for each, add an entry to the table. Also,
            keep track of the names of the variables. *)
         let linear_names: identifier list =
           List.filter_map (fun (TypedBinding { rename; _ }) ->
               Some rename)
             bindings
         in
         let tbl: state_tbl =
           Util.iter_with_context
             (fun tbl (TypedBinding { rename; ty; _ }) ->
               add_entry tbl rename ty depth)
             tbl
             bindings
         in
         let tbl: state_tbl = check_stmt tbl depth body in
         (* Once we leave the scope, remove the linear variables we added. *)
         let tbl: state_tbl = remove_entries tbl linear_names in
         tbl)
  | TAssign (span, lvalue, expr, first) ->
     adorn_error_with_span span
       (fun _ ->
         (* Linear values can't be consumed in an L-value, because the only place
            where they could appear is as the index value to an array indexing
            operator (e.g. `[foo(x)]`) and that wouldn't typecheck in the first
            place. If the lvalue is a variable we can assign to it however, if it's been
            consumed. *)
         let (TypedLValue (var_name, path_elems)) = lvalue in
         let tbl: state_tbl = check_expr tbl depth expr in
         (match get_entry tbl var_name with
          | Some (ty, _, state) ->
             (match path_elems with
              | [] ->
                 (* Assigning to a variable. *)
                 (match state with
                  | Unconsumed | BorrowedRead | BorrowedWrite ->
                     if universe_linear_ish (type_universe ty) then
                       if first then
                         tbl
                       else
                          austral_raise LinearityError [
                              Text "Cannot assign to the variable ";
                              Code (ident_string var_name);
                              Text " because it is not yet consumed."
                            ]
                     else
                       tbl
                  | Consumed ->
                     let tbl: state_tbl = update_tbl tbl var_name Unconsumed in
                     if is_pending tbl var_name then
                       remove_pending tbl var_name
                     else
                       tbl)
              | _ ->
                 (* Assigning to a path. *)
                 tbl)
          | None ->
             tbl))
  | TIf (span, cond, tb, fb) ->
     adorn_error_with_span span
       (fun _ ->
         let tbl: state_tbl = check_expr tbl depth cond in
         let true_tbl: state_tbl = check_stmt tbl depth tb in
         let false_tbl: state_tbl = check_stmt tbl depth fb in
         let _ = tables_are_consistent "an if" true_tbl false_tbl in
         true_tbl)
  | TCase (span, expr, whens, _) ->
     adorn_error_with_span span
       (fun _ ->
         let tbl: state_tbl = check_expr tbl depth expr in
         let tbls: state_tbl list = check_whens tbl depth whens in
         let _ = table_list_is_consistent tbls in
         (match tbls with
          | first::rest ->
             let _ = rest in
             first
          | [] ->
             tbl))
  | TWhile (span, cond, body) ->
     adorn_error_with_span span
       (fun _ ->
         let pending_before: identifier list = get_pending tbl in
         let tbl: state_tbl = check_expr tbl depth cond in
         let tbl: state_tbl = check_stmt tbl (depth + 1) body in
         let pending_after: identifier list = get_pending tbl in
         (* pending_after - pending_before = variables consumed in the loop and not
            assigned to *)
         let new_pending: identifier list = subtract pending_after pending_before in
         if (List.length new_pending) > 0 then
           pending_error new_pending
         else
           tbl)
  | TFor (span, _, start, final, body) ->
     adorn_error_with_span span
       (fun _ ->
         let tbl: state_tbl = check_expr tbl depth start in
         let tbl: state_tbl = check_expr tbl depth final in
         let pending_before: identifier list = get_pending tbl in
         let tbl: state_tbl = check_stmt tbl (depth + 1) body in
         let pending_after: identifier list = get_pending tbl in
         let new_pending: identifier list = subtract pending_after pending_before in
         if (List.length new_pending) > 0 then
           pending_error new_pending
         else
           tbl)
  | TBorrow { original; mode; body; rename; ref_type; span; _ } ->
     adorn_error_with_span span
       (fun _ ->
         (* Ensure the original variable is unconsumed to be borrowed. *)
         if is_unconsumed tbl original then
           let tbl: state_tbl =
             match mode with
             | Read ->
                (* Mark the original as being read-borrowed. *)
                update_tbl tbl original BorrowedRead
             | Write ->
                (* Mark the original as being write-borrowed. *)
                update_tbl tbl original BorrowedWrite
             | Reborrow ->
                (* Mark the original as being write-borrowed. *)
                update_tbl tbl original BorrowedWrite
           in
           (* If it's a mutable borrow, or a reborrow, add the reference variable. *)
           let tbl: state_tbl =
             match mode with
             | Write ->
                add_entry tbl rename ref_type depth
             | Reborrow ->
                add_entry tbl rename ref_type depth
             | Read ->
                tbl
           in
           (* Traverse the body. *)
           let tbl: state_tbl = check_stmt tbl depth body in
           (* After the body, unborrow the variable. *)
           let tbl: state_tbl = update_tbl tbl original Unconsumed in
           (* If it's a mutable borrow, or a reborrow, remove the reference from the state table. *)
           let tbl: state_tbl =
             match mode with
             | Read ->
                tbl
             | Write ->
                remove_entry tbl rename
             | Reborrow ->
                remove_entry tbl rename
           in
           tbl
         else
           (* If we're read-borrowing, and the variable is already read-borrowed, that's also allowed. *)
           let state: var_state = get_state tbl original in
           if (mode = Read) && (state = BorrowedRead) then
             (* Traverse the body. *)
             let tbl: state_tbl = check_stmt tbl depth body in
             (* After the body, unborrow the variable. *)
             let tbl: state_tbl = update_tbl tbl original Unconsumed in
             tbl
           else
            austral_raise LinearityError [
                  Text "Cannot borrow the variable ";
                  Code (ident_string original);
                  Text " because it is already ";
                  Text (humanize_state state);
                  Text "."
               ])
  | TBlock (_, a, b) ->
     let tbl: state_tbl = check_stmt tbl depth a in
     let tbl: state_tbl = check_stmt tbl depth b in
     tbl
  | TDiscarding (span, expr) ->
     adorn_error_with_span span
       (fun _ ->
         let tbl: state_tbl = check_expr tbl depth expr in
         tbl)
  | TReturn (span, expr) ->
     adorn_error_with_span span
       (fun _ ->
         let tbl: state_tbl = check_expr tbl depth expr in
         (* Ensure that all variables are Consumed. *)
         let _ =
           List.map (fun (name, ty, _, state) ->
               if state = Consumed then
                 ()
               else
                 (* Is the type of this variable a write reference? *)
                 (match ty with
                  | WriteRef _ ->
                     (* Write references can be dropped implicitly. *)
                     ()
                  | _ ->
                     if universe_linear_ish (type_universe ty) then
                       austral_raise LinearityError [
                           Text "The variable ";
                           Code (ident_string name);
                           Text " is not consumed by the time of the return statement. Did you forget to call a destructor, or destructure the contents?"
                         ]
                     else
                       ()))
             (tbl_to_list tbl)
         in
         tbl)

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
          add_entry tbl rename ty depth
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
      add_entry tbl name ty 0
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
