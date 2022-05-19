(** The table of appearances is used by the lifetime analysis pass. *)
open Identifier
open Ptast

(** The table of appearances maps linear variables to the position where they
    were defined, the list of places where they appear, and the loop context
    where the variable was defined. *)
type appear_tbl

(** A loop is either a while loop or a for loop. *)
type loop_kind = CtxWhile | CtxFor

(** A loop context is best explained with an example. Consider:

    while ... do
        let x: T := Make_T();
        for i from 0 to n do
            while ... do
                consume(x);
            end while;
        end for;
    end while;

    Here, the loop context where `x` is defined is `[CtxWhile]`, and the loop
    context where `x` is consumed is `[CtxWhile, CtxFor, CtxWhile]`. This is
    used to verify that a linear variable does not get consumed inside a loop.
 *)
type loop_context = loop_kind list

(** A linear variable can appear in three ways: being consumed, being at the
    head of a path, or being borrowed. *)
type appear_kind = AppearConsume | AppearPath | AppearBorrow

(** The empty table. *)
val empty_appearances : appear_tbl

(** Given a variable's name, the position where it was defined, and the loop
    context where it is defined, add a row to the table of appearances. Errors
    if a variable with that name already exists. *)
val register_var : appear_tbl -> identifier -> pos -> loop_context -> appear_tbl

(** Given a variable's name, register an appearance with a position, type, and
    the loop context where the variable appeared. *)
val register_appear : appear_tbl -> identifier -> pos -> appear_kind -> loop_context -> appear_tbl

(** Return whether the given name exists in the table. *)
val tbl_has_name : appear_tbl -> identifier -> bool
