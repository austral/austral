(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Type
open Util

(** An event: either entering a frame, printing a value, or leaving a frame. *)
type event =
  | EnterFrame of string
  | LeaveFrame
  | PrintValue of string * string

(** The global event list. *)
let events: event list ref = ref []

(** Add an event to the event list. *)
let push_event (event: event): unit =
  events := (event :: !events)

let prefix: string = "<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>Austral Compiler Call Tree</title>
  </head>
  <body>
<h1>Compiler Call Tree</h1>
<ul>\n"

let suffix: string = "</ul>
    <style>
      .multi-line {
        display: flex;
        flex-direction: row;
      }

      .frame-contents {
        display: none;
      }

      .frame-contents.show {
        display: block;
      }
    </style>
    <script>
      const triggers = Array.from(document.querySelectorAll('[data-toggle]'));

      window.addEventListener('click', (ev) => {
        const elm = ev.target;
        if (triggers.includes(elm)) {
          const frameId = elm.getAttribute('data-toggle');
          collapse(frameId);
        }
      }, false);

      const collapse = (frameId) => {
        const target = document.querySelector('#' + frameId);
        target.classList.toggle('show');
      }
    </script>
  </body>
</html>"

(** Used to match frame titles with frame contents for toggling. *)
let toggle_id = ref 0

let fresh_toggle_id _: int =
  let id = !toggle_id in
  toggle_id := id + 1;
  id

(** Render the event list to HTML. *)
let rec render _ : string =
  prefix
 ^ (String.concat "" (List.map render_event (List.rev !events)))
 ^ suffix

and render_event (event: event): string =
  match event with
  | EnterFrame name ->
     let id: string = string_of_int (fresh_toggle_id ()) in
     "<li class='frame'>
<div class='frame-title' data-toggle='frame-" ^ id ^ "'>" ^ name ^ "</div>
<ul class='frame-contents' id='frame-" ^ id ^ "'>\n"
  | LeaveFrame ->
     "</ul>
</li>\n"
  | PrintValue (label, value) ->
     (match String.index_opt value '\n' with
      | Some _ ->
         (* Has a newline. *)
         ("<li class='prop'>
<div class='multi-line'>
<span class='label'>" ^ label ^ "</span>
<pre class='value'><code>" ^ value ^ "</pre></code>
</div>
</li>\n")
      | None ->
         (* No newline. *)
         ("<li class='prop'>
<div class='single-line'>
<span class='label'>" ^ label ^ "</span>
<code class='value'>" ^ value ^ "</code>
</div>
</li>\n"))


let dump _: unit =
  write_string_to_file "calltree.html" (render ())

(* Below are functions to push events. *)

let with_frame (name: string) (f: unit -> 'a): 'a =
  push_event (EnterFrame name);
  let result: 'a = f () in
  push_event LeaveFrame;
  result

type label = string

let ps ((label, value): (label * string)): unit =
  push_event (PrintValue (label, value))

let pi ((label, value): (label * identifier)): unit =
  push_event (PrintValue (label, ident_string value))

let pqi ((label, value): (label * qident)): unit =
  push_event (PrintValue (label, qident_debug_name value))

let pt ((label, value): (label * ty)): unit =
  push_event (PrintValue (label, type_string value))
