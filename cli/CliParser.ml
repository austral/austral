(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Austral_core
open Identifier
open CliUtil
open Error

type entrypoint =
  | Entrypoint of module_name * identifier
[@@deriving eq]

type mod_source =
  | ModuleSource of { inter_path: string; body_path: string }
  | ModuleBodySource of { body_path: string }
[@@deriving eq]

type target =
  | TypeCheck
  | Executable of { bin_path: string; entrypoint: entrypoint; }
  | CStandalone of { output_path: string; entrypoint: entrypoint option; }
[@@deriving eq]

type error_reporting_mode =
  | ErrorReportPlain
  | ErrorReportJson
[@@deriving eq]

type cmd =
  | HelpCommand
  | VersionCommand
  | CompileHelp
  | WholeProgramCompile of {
      modules: mod_source list;
      target: target;
      error_reporting_mode: error_reporting_mode;
    }
[@@deriving eq]

let check_leftovers (arglist: arglist): unit =
  if (arglist_size arglist) > 0 then
    err "There are leftover arguments."
  else
    ()

let parse_mod_source (s: string): mod_source =
  let ss = String.split_on_char ',' s in
  match ss with
  | [path] ->
     ModuleBodySource { body_path = path }
  | [inter_path; body_path] ->
     ModuleSource { inter_path = inter_path; body_path = body_path }
  | _ ->
     cli_error (InvalidModuleSource s)

let parse_entrypoint (s: string): entrypoint =
  let ss = String.split_on_char ':' s in
  match ss with
  | [mn; i] ->
     Entrypoint (make_mod_name mn, make_ident i)
  | _ ->
     cli_error (InvalidEntrypoint s)

let parse_executable_target (arglist: arglist): (arglist * target) =
  (* Get the --entrypoint *)
  match pop_value_flag arglist "entrypoint" with
  | Some (arglist, entrypoint) ->
     (match pop_value_flag arglist "output" with
      | Some (arglist, bin_path) ->
         (arglist, Executable { bin_path = bin_path; entrypoint = parse_entrypoint entrypoint })
      | None ->
         cli_error MissingOutput)
  | None ->
     (match pop_bool_flag arglist "--no-entrypoint" with
      | Some _ ->
         cli_error NoEntrypointWrongTarget
      | None ->
         cli_error MissingEntrypoint)

let get_output (arglist: arglist): (arglist * string) =
  match pop_value_flag arglist "output" with
  | Some (arglist, output_path) ->
     (arglist, output_path)
  | None ->
     cli_error MissingOutput

let parse_c_target (arglist: arglist): (arglist * target) =
  (* Get the --entrypoint *)
  match pop_value_flag arglist "entrypoint" with
  | Some (arglist, entrypoint) ->
     (* An entrypoint was passed in. *)
     let (arglist, output_path) = get_output arglist in
     (arglist, CStandalone { output_path = output_path; entrypoint = Some (parse_entrypoint entrypoint) })
  | None ->
     (* No --entrypoint. Did we get the --no-entrypoint flag? *)
     (match pop_bool_flag arglist "no-entrypoint" with
      | Some arglist ->
         let (arglist, output_path) = get_output arglist in
         (arglist, CStandalone { output_path = output_path; entrypoint = None })
      | None ->
         cli_error MissingEntrypoint)

let parse_target_type (arglist: arglist): (arglist * target) =
  match pop_value_flag arglist "target-type" with
  | Some (arglist, target_value) ->
     (* An explicit target type was passed. *)
     (match target_value with
      | "exe" ->
         (* Build an executable binary. *)
         parse_executable_target arglist
      | "c" ->
         (* Build a standaloine C file. *)
         parse_c_target arglist
      | "tc" ->
         (* Typecheck. *)
         (arglist, TypeCheck)
      | _ ->
         cli_error (UnknownTarget target_value))
  | None ->
     (* The default target is to build an executable binary. This means we need
        an entrypoint. *)
     parse_executable_target arglist

let parse_error_reporting_mode (arglist: arglist): (arglist * error_reporting_mode) =
  match pop_value_flag arglist "error-format" with
  | Some (arglist, value) ->
     (* An explicit error reporting mode was passed. *)
     (match value with
      | "plain" ->
         (* Report errors in plain text. *)
         (arglist, ErrorReportPlain)
      | "json" ->
         (* Report errors in JSON. *)
         (arglist, ErrorReportJson)
      | _ ->
         cli_error (UnknownErrorReportingMode value))
  | None ->
     (* The default target is plain text errors. *)
     (arglist, ErrorReportPlain)

let parse_compile_command' (arglist: arglist): (arglist * cmd) =
  (* Parse module list *)
  let (arglist, modules): (arglist * string list) = pop_positional arglist in
  let modules: mod_source list = List.map parse_mod_source modules in
  let (arglist, error_reporting_mode) = parse_error_reporting_mode arglist in
  (* There must be at least one module. *)
  if ((List.length modules) < 1) then
    cli_error MissingModule
  else
    (* Parse the target type. *)
    let (arglist, target): (arglist * target) = parse_target_type arglist in
    (arglist, WholeProgramCompile { modules = modules; target = target; error_reporting_mode = error_reporting_mode; })

let parse_compile_command (arglist: arglist): (arglist * cmd) =
  match pop_bool_flag arglist "help" with
  | Some arglist ->
     (arglist, CompileHelp)
  | None ->
     parse_compile_command' arglist

let parse (arglist: arglist): cmd =
  let args: arg list = arglist_to_list arglist in
  match args with
  | [BoolFlag "help"] ->
     HelpCommand
  | [BoolFlag "version"] ->
     VersionCommand
  | (PositionalArg "compile")::rest ->
     (* Try parsing the `compile` command. *)
     let (arglist, cmd) = parse_compile_command (arglist_from_list rest) in
     let _ = check_leftovers arglist in
     cmd
  | _ ->
     HelpCommand
