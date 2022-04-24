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

(** Render the event list to HTML. *)
let rec render _ : string =
  String.concat "" (List.map render_event !events)

and render_event (event: event): string =
  match event with
  | EnterFrame name ->
     "<li class='frame'>
<div class='frame-title'>" ^ name ^ "</div>
<ul class='frame-contents'>\n"
  | LeaveFrame ->
     "</ul>
</li>\n"
  | PrintValue (label, value) ->
     "<li class='value'>
<span class='label'>" ^ label ^ "</span>
<pre><code>" ^ value ^ "</pre></code>
</li>\n"

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
