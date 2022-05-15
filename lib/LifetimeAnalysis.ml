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
  | PLet of pos * span * identifier * ty * texpr * tstmt
  | PLetBorrow of {
      pos: pos;
      span: span;
      name: identifier;
      ty: ty;
      region_name: identifier;
      region: region;
      var_name: identifier;
      mode: borrowing_mode;
      body: tstmt;
    }
  | PDestructure of pos* span * (identifier * ty) list * texpr * tstmt
  | PAssign of span * pos * typed_lvalue * pos * texpr
  | PIf of pos * span * texpr * tstmt * tstmt
  | PCase of pos * span * texpr * typed_when list
  | PWhile of pos * span * texpr * tstmt
  | PFor of span * identifier * pos * texpr * pos * texpr * tstmt
  | PBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: ty;
      ref_type: ty;
      body: tstmt;
      mode: borrowing_mode
    }
  | PBlock of span * tstmt * tstmt
  | PDiscarding of pos * span * texpr
  | PReturn of pos * span * texpr
