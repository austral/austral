open Error

type arg =
  | BoolFlag of string
  | ValueFlag of string * string
  | PositionalArg of string

type arglist = ArgList of arg list

let parse_arg (arg: string): arg =
  if String.starts_with ~prefix:"--" arg then
    (* It's a flag. *)
    match String.split_on_char '=' arg with
    | [name] ->
       (* It's a boolean flag, e.g. --help *)
       BoolFlag name
    | [name; value] ->
       (* It's a value flag, e.g. --foo=bar *)
       ValueFlag (name, value)
    | _ ->
       err ("Invalid flag argument: '" ^ arg ^ "'")
  else
    (* It's a positional flag. *)
    PositionalArg arg

let parse_args (args: string list): arglist =
  (** At least one argument, the name of the binary. *)
  if ((List.length args) < 1) then
    err "Argument list must have at least one element in it, the path to the binary. Was the compiler invoked through a system call?"
  else
    ArgList (List.map parse_arg (List.tl args))

let arglist_size (ArgList l): int =
  List.length l

let arg_has_name (arg: arg) (name: string): bool =
  match arg with
  | BoolFlag n ->
     name = n
  | ValueFlag (n, _) ->
     name = n
  | PositionalArg _ ->
     false

let flags_with_name (arglist: arglist) (name: string): arg list =
  let (ArgList l) = arglist in
  List.filter (fun a -> arg_has_name a name) l

let args_without_name (arglist: arglist) (name: string): arglist =
  let (ArgList l) = arglist in
  ArgList (List.filter (fun a -> not (arg_has_name a name)) l)

let pop_flag (arglist: arglist) (name: string): (arglist * arg) option =
  (* Find all flags with the name. *)
  let flags: arg list = flags_with_name arglist name
  (* Find all args without the name. *)
  and args_rest: arglist = args_without_name arglist name
  in
  match flags with
  | [flag] ->
     (* One flag with the name. *)
     Some (args_rest, flag)
  | _::_ ->
     (* Multiple flags with the name. *)
     err "Duplicate flag."
  | [] ->
     (* No such flag. *)
     None

let pop_bool_flag (arglist: arglist) (name: string): arglist option =
  match pop_flag arglist name with
  | Some (arglist, flag) ->
     (match flag with
      | BoolFlag _ ->
         Some arglist
      | _ ->
         err "Expected bool flag, got value flag.")
  | None ->
     None

let pop_value_flag (arglist: arglist) (name: string): (arglist * string) option =
  match pop_flag arglist name with
  | Some (arglist, flag) ->
     (match flag with
      | ValueFlag (_, value) ->
         Some (arglist, value)
      | _ ->
         err "Expected value flag, got boolean flag.")
  | None ->
     None

let is_pos (arg: arg): bool =
  match arg with
  | PositionalArg _ -> true
  | _ -> false

let pos_name (arg: arg): string =
  match arg with
  | PositionalArg value -> value
  | _ -> internal_err "pos_name called with non-positional argument."

let pop_positional (arglist: arglist): (arglist * string list) =
  let (ArgList l) = arglist in
  let pos: arg list = List.filter is_pos l
  and nonpos: arg list = List.filter (fun a -> not (is_pos a)) l in
  (ArgList nonpos, List.map pos_name pos)

let get_positional (arglist: arglist): string list =
  let (ArgList l) = arglist in
  List.map pos_name (List.filter is_pos l)

let adjust_positional (arglist: arglist) (pos: string list): arglist =
  let (arglist, _) = pop_positional arglist in
  let (ArgList flags) = arglist in
  ArgList (List.concat [flags; List.map (fun a -> PositionalArg a) pos])
