open Identifier
open Common
open Span
open Tast
open Type
open Region

(** Positions are just integer indices. *)
type pos = int

(** Typed statements with position tracking. *)
type pstmt =
  | PSkip of span
  | PLet of pos * span * identifier * ty * texpr * pstmt
  | PLetBorrow of {
      pos: pos;
      span: span;
      name: identifier;
      ty: ty;
      region_name: identifier;
      region: region;
      var_name: identifier;
      mode: borrowing_mode;
      body: pstmt;
    }
  | PDestructure of pos * span * (identifier * ty) list * texpr * pstmt
  | PAssign of span * pos * typed_lvalue * pos * texpr
  | PIf of pos * span * texpr * pstmt * pstmt
  | PCase of pos * span * texpr * typed_when list
  | PWhile of pos * span * texpr * pstmt
  | PFor of span * identifier * pos * texpr * pos * texpr * pstmt
  | PBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: pstmt;
      mode: borrowing_mode
    }
  | PBlock of span * pstmt * pstmt
  | PDiscarding of pos * span * texpr
  | PReturn of pos * span * texpr

let current_pos = ref 0

let fresh_pos _ =
  let p = !current_pos in
  current_pos := p + 1;
  p

let reset_pos _ =
  current_pos := 0

let rec add_pos (stmt: tstmt): pstmt =
  match stmt with
  | TSkip span ->
     PSkip span
  | TLet (span, name, ty, value, body) ->
     let pos = fresh_pos () in
     PLet (pos, span, name, ty, value, add_pos body)
  | TLetBorrow { span; name; ty; region_name; region; var_name; mode; body } ->
     let pos = fresh_pos () in
     PLetBorrow { pos; span; name; ty; region_name; region; var_name; mode; body=add_pos body }
  | TDestructure (span, bindings, value, body) ->
     let pos = fresh_pos () in
     PDestructure (pos, span, bindings, value, add_pos body)
  | TAssign (span, lvalue, value) ->
     let pos = fresh_pos () in
     let pos' = fresh_pos () in
     PAssign (span, pos, lvalue, pos', value)
  | TIf (span, test, tb, fb) ->
    let pos = fresh_pos () in
    let tb = add_pos tb in
    let fb = add_pos fb in
    PIf (pos, span, test, tb, fb)
  | TCase (span, value, whens) ->
     let pos = fresh_pos () in
     PCase (pos, span, value, whens)
  | TWhile (span, test, body) ->
     let pos = fresh_pos () in
     PWhile (pos, span, test, add_pos body)
  | TFor (span, name, initial, final, body) ->
     let pos = fresh_pos () in
     let pos' = fresh_pos () in
     PFor (span, name, pos, initial, pos', final, add_pos body)
  | TBorrow { span; original; rename; region; orig_type; ref_type; body; mode } ->
     PBorrow { span; original; rename; region; orig_type; ref_type; body=add_pos body; mode }
  | TBlock (span, a, b) ->
     let a = add_pos a in
     let b = add_pos b in
     PBlock (span, a, b)
  | TDiscarding (span, value) ->
     PDiscarding (fresh_pos (), span, value)
  | TReturn (span, value) ->
     PReturn (fresh_pos (), span, value)

(** Add positions to a typed statement. *)
let track_positions (stmt: tstmt): pstmt =
  reset_pos ();
  let stmt = add_pos stmt in
  reset_pos ();
  stmt
