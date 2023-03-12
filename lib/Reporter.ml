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
    <button onclick='openAll()'>Open All</button>
    <button onclick='closeAll()'>Close All</button>

<div class=tree>
<ul>\n"

let suffix: string = "</ul>
    </div>
    <script>
        function openAll() {
            trees = document.getElementsByClassName('tree');
            for (const tree of trees) {
                for (const det of tree.getElementsByTagName('details')) {
                    det.open = true;
                }
            }
        }

        function closeAll() {
            trees = document.getElementsByClassName('tree');
            for (const tree of trees) {
                for (const det of tree.getElementsByTagName('details')) {
                    det.removeAttribute('open');
                }
            }
        }
    </script>
    <style>
        .tree {
            --step: 25px;
            --height: 16px;
            --wire-width: 2px;
            --tree-color: #999;
        }
        .tree ul {
            padding-left: 0;
        }
        .tree ul li {
            padding-left: calc(var(--step));
            border: none;
            display: block; /* remove bullets */
            position: relative;
            margin-right: none;
        }

        .tree ul li::before, .tree ul li::after{
            content: '';
            display: block;
            position: absolute;
            left: calc(0px - var(--wire-width));
            top: calc(0px - var(--height)/2);
            width: calc(var(--step));
            height: calc(var(--height));
            border: calc(var(--wire-width)) solid var(--tree-color);
            border-top: transparent;
            border-right: transparent;
        }

        .tree ul li::after {
            border-top: transparent;
            height: 100%;
            width: 0px;
        }

        .tree ul li:last-child::after {
            border-left: transparent;
        }

        .tree summary {
            display : block;
            margin-left: calc(var(--height));
        }
        .tree summary::before {
            content: '+';
            display: block;
            position: absolute;
            left: calc(var(--step) - var(--height)/2);
            width: calc(var(--height));
            height: calc(var(--height));
            border: none;
            border-radius: 100%;
            background-color: var(--tree-color);
            z-index: 2;
            color: white;
            text-align: center;
        }
        .tree details[open] > summary::before {
            content: '-';
        }
    </style>
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
<details>
<summary frame-id=" ^ id ^ "'>" ^ name ^ "</summary>
<ul class='frame-contents' data-toggle='frame-" ^ id ^ "'>\n"
  | LeaveFrame ->
"</ul>
</details>
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
