open Identifier
open Ptast
open Error
open Region

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

type region_lifetime = RegLifetime of region * pos * pos option

type lifetime_tbl = region_lifetime list

let get_region_lifetime (tbl: lifetime_tbl) (r: region): region_lifetime option =
  List.find_opt (fun (RegLifetime (r', _, _)) -> equal_region r r') tbl

let pop_region_lifetime (tbl: lifetime_tbl) (r: region): (region_lifetime * lifetime_tbl) option =
  match get_region_lifetime tbl r with
  | Some rl ->
     let tbl' = List.filter (fun (RegLifetime (r', _, _)) -> not (equal_region r r')) tbl
     in
     Some (rl, tbl')
  | None ->
     None

let register_region (tbl: lifetime_tbl) (r: region) (start: pos): lifetime_tbl =
  match get_region_lifetime tbl r with
  | Some _ ->
     err "Region already exists in table of lifetimes."
  | None ->
     (RegLifetime (r, start, None)) :: tbl

let update_region_end (tbl: lifetime_tbl) (r: region) (end_pos: pos): lifetime_tbl =
  match pop_region_lifetime tbl r with
  | Some (RegLifetime (_, start_pos, end_pos'), tbl') ->
     (match end_pos' with
      | None ->
         (* Any position is greater than None, so update. *)
         RegLifetime (r, start_pos, Some end_pos) :: tbl'
      | Some end_pos'' ->
         (* If there's an existing end position, update it only if the
            client-provided one is larger. *)
         if end_pos > end_pos'' then
           RegLifetime (r, start_pos, Some end_pos) :: tbl'
         else
           tbl)
  | None ->
     err "Region does not exist in table of lifetimes."
