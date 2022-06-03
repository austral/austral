open Identifier
open Tast
open Type
open TypeSystem
open Error

(* Data structures *)

type loop_depth = int

type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed

type state_tbl = (identifier * loop_depth * var_state) list

let empty_tbl: state_tbl = []

let get_entry (tbl: state_tbl) (name: identifier): (loop_depth * var_state) option =
  match (List.find_opt (fun (n, _,_) -> equal_identifier name n) tbl) with
  | Some (_, depth, state) ->
     Some (depth, state)
  | None ->
     None

let add_entry (tbl: state_tbl) (name: identifier) (depth: loop_depth): state_tbl =
  match get_entry tbl name with
  | None ->
     (name, depth, Unconsumed) :: tbl
  | Some _ ->
     err "An entry exists in the state table with this name."

let update_tbl (tbl: state_tbl) (name: identifier) (state: var_state): state_tbl =
  match get_entry tbl name with
  | None ->
     err "No variable in the state table with this name."
  | Some (depth, _) ->
     let other_entries = List.filter (fun (n, _,_) -> not (equal_identifier name n)) tbl
     in
     (name, depth, state) :: other_entries

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
  | TArithmetic (_, lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TFuncall (_, _, args, _, _) ->
     merge_list (List.map c args)
  | TMethodCall (_, _, _, args, _, _) ->
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
  | TTypeAliasConstructor (_, e) ->
     c e
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

(* Linearity checking *)

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
        err "Impossible"

let rec linearity_check (params: value_parameter list) (body: tstmt): unit =
  (* Initialize the loop depth to zero, *)
  let loop_depth: int ref = ref 0 in
  (* Initialize the state table to the empty table. *)
  let tbl: state_tbl ref = ref empty_tbl in
  (* Populate the table with the linear parameters. *)
  tbl := init_tbl !tbl params;
  (* Traverse the code in execution order. *)
  let rec check (stmt: tstmt): unit =
    match stmt with
    | TSkip _ ->
       ()
    | TDiscarding (_, expr) ->
       check_expr expr;
    | _ ->
       err "Not implemented yet"
  and check_expr (expr: texpr): unit =
    (* For each variable in the table, check if the variable is used correctly
       in the expression. *)
    let _ = List.map (fun (name, _, _) -> check_var_in_expr name expr) (tbl_to_list !tbl)
    in
    ()
  and check_var_in_expr (name: identifier) (expr: texpr): unit =
    (* Count the appearances of the variable in the expression. *)
    let apps: appearances = count name expr in
    let { consumed: int; read: int; write: int; path: int } = apps in
    (* Perform the checks *)
    match partition consumed with
    | MoreThanOne ->
       (* The variable is consumed more than once: signal an error. *)
       err "The variable is consumed more than once."
    | One ->
       (* The variable is consumed exactly once. Check that:

          1. x is Unconsumed.

          2. `read`, `write`, and `path` are zero.

          3. the current loop depth is the same as the depth where the
          variable is defined.

        *)
       if (is_unconsumed name) then
         if ((read = 0) && (path = 0)) then
           if (!loop_depth) = (get_loop_depth name) then
             (* Everything checks out. Mark the variable as consumed. *)
             tbl := update_tbl !tbl name Consumed
           else
             err "Loop depth mismatch."
         else
           err "Cannot consume a variable in the same expression as it is borrowed or accessed through a path."
       else
         err "Variable already consumed."
    | Zero ->
       (* The variable is not consumed. *)
       (match partition write with
        | MoreThanOne ->
           (* The variable is borrowed mutably more than once. Signal an error. *)
           err "We can't borrow mutably more than once within a single expression."
        | One ->
           (* The variable was borrowed mutably once. Check that:

              1. It is unconsumed.
              2. `read`, `path` are zero. *)
           if is_unconsumed name then
             if ((read = 0) && (path = 0)) then
               (* Everything checks out. *)
               ()
             else
               (* Signal an error: cannot borrow mutably while also borrowing
                  immutably or reading through a path. *)
               err ""
           else
             err "Variable already consumed."
        | Zero ->
           (* The variable is neither consumed nor mutably borrowed, so we can
              read it (borrow read-only or access through a path) iff it is
              unconsumed. *)
           if read > 0 then
             (* If the variable is borrowed read-only, ensure it is unconsumed. *)
             if is_unconsumed name then
               (* Everything checks out. *)
               ()
             else
               err "Cannot borrow a variable: state mismatch."
           else
             if path > 0 then
               (* If the variable is accessed through a path, ensure it is unconsumed. *)
               if is_unconsumed name then
                 (* Everything checks out. *)
                 ()
               else
                 err "Cannot use variable as the head of a path: state mismatch."
             else
               (* The variable is not used in this expression. *)
               ())
  and get_loop_depth (name: identifier): loop_depth =
    match get_entry !tbl name with
    | Some (depth, _) ->
       depth
    | _ ->
       err "Not in table"
  and is_unconsumed (name: identifier): bool =
    match get_entry !tbl name with
    | Some (_, state) ->
       (match state with
        | Unconsumed -> true
        | _ -> false)
    | _ ->
       err "Not in table"
  in
  check body;

and init_tbl (tbl: state_tbl) (params: value_parameter list): state_tbl =
  let f (tbl: state_tbl) (ValueParameter (name, ty)): state_tbl =
    if universe_linear_ish (type_universe ty) then
      (* Add this parameter to the list at depth zero. *)
      add_entry tbl name 0
    else
      tbl
  in
  Util.iter_with_context f tbl params

and universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | _ -> false
