open Identifier
open Ptast
open Error

type appear_tbl = appear_row list

and appear_row = Row of identifier * pos * loop_context * appear list

and appear = Appear of pos * appear_kind * loop_context

and loop_context = loop_kind list

and loop_kind = CtxWhile | CtxFor

and appear_kind = AppearConsume | AppearPath | AppearBorrow

let empty_appearances: appear_tbl = []

let get_var (tbl: appear_tbl) (name: identifier): appear_row option =
  List.find_opt (fun (Row (name', _, _, _)) -> equal_identifier name name') tbl

(** Given a variable's name, pop its row from the table, returning the row and
    the table without it. *)
let pop_var (tbl: appear_tbl) (name: identifier): (appear_row * appear_tbl) option =
  match get_var tbl name with
  | Some row ->
     Some (row, List.filter (fun (Row (name', _, _, _)) -> not (equal_identifier name name')) tbl)
  | None ->
     None

let register_var (tbl: appear_tbl) (name: identifier) (pos: pos) (ctx: loop_context): appear_tbl =
  match get_var tbl name with
  | Some _ ->
     err "Duplicate entry in the table of appearances"
  | None ->
     Row (name, pos, ctx, []) :: tbl

let register_appear (tbl: appear_tbl) (name: identifier) (pos: pos) (kind: appear_kind) (ctx: loop_context): appear_tbl =
  match pop_var tbl name with
  | Some (Row (_, init_pos, init_ctx, appearances), tbl) ->
     (Row (name, init_pos, init_ctx, Appear (pos, kind, ctx) :: appearances)) :: tbl
  | None ->
     err "No variable with that name."

let tbl_has_name (tbl: appear_tbl) (name: identifier): bool =
  Option.is_some (get_var tbl name)
