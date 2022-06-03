open Identifier
open Error

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
