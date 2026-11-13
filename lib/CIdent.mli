(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The names in the generated C. A name is made from where it comes from, and
    [render] alone decides how it is spelled in C. *)

(** A C identifier. *)
type t

(** A name the compiler builds (a declaration id, a monomorph id, a temporary)
    or one the prelude defines. It is spelled as it is: no such name starts
    with a C reserved word. *)
val generated : string -> t

(** A name the user wrote in Austral: a variable, a parameter. It is spelled
    with the reserved-word escape. *)
val austral : string -> t

(** A name the user gave in a pragma for a C symbol. It is spelled as it is. *)
val external_name : string -> t

(** A name the user wrote in Austral that reaches C without the reserved-word
    escape: a record slot, a union case, a parameter in the [extern]
    declaration of a foreign function, a constant where it is defined. A slot
    called [double] breaks the C compile (issue #92). The fix for that issue
    replaces every use with [austral]. *)
val austral_unescaped : string -> t

(** A name from a pragma that reaches C with the reserved-word escape: a
    foreign function where it is called, the wrapper of an exported function.
    [fork] is declared as [fork] and called as [au_ckw_fork] (issue #92). The
    fix for that issue replaces every use with [external_name]. *)
val external_escaped : string -> t

(** Whether two names are spelled the same in C. *)
val equal : t -> t -> bool

(** The spelling in C. The reserved-word escape puts [au_ckw_] before a name
    that starts with a C reserved word: before [double], and before [doubled]
    too. *)
val render : t -> string
