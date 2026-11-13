(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors with the user's command line arguments, and their wording. *)
open ErrorText

type t =
  | InvalidEntrypoint of string
  | InvalidModuleSource of string
  | MissingEntrypoint
  | MissingModule
  | MissingOutput
  | NoEntrypointWrongTarget
  | UnknownTarget of string
  | UnknownErrorReportingMode of string
  | DuplicateFlags of string
  | EmptyArgs
  | ExpectedBoolFlag of string
  | ExpectedValueFlag of string
  | UnknownFlagType of string
  | CCompilerError of { command: string; exit_code: int; stdout: string; stderr: string }
  | WriteError of { path: string; reason: string option }

let render (error: t): err_text =
  match error with
  | InvalidEntrypoint entry ->
     [
       Text "Invalid entrypoint format ";
       Code entry;
       Break;
       Text "The entrypoint must be supplied in the form ";
       Code "Module:name"
     ]
  | InvalidModuleSource source ->
     [
       Text "Invalid module source format ";
       Code source;
       Break;
       Text "Sources must be supplied as either";
       Code "interface.aui,body.aum";
       Text " or ";
       Code "body.aum"
     ]
  | MissingEntrypoint ->
     [
       Code "--entrypoint";
       Text " argument not provided."
     ]
  | MissingModule ->
     [
       Text "The ";
       Code "compile";
       Text " command must specify at least one module."
     ]
  | MissingOutput ->
     [
       Code "--output";
       Text " argument not provided."
     ]
  | NoEntrypointWrongTarget ->
     [
       Code "--no-entrypoint";
       Text " requires ";
       Code "--target-type=c";
       Text ", because otherwise the compiler will try to build the generated C code, and will fail because there is no entrypoint function."
     ]
  | UnknownTarget target ->
     [
       Text "Unknown target type ";
       Code target
     ]
  | UnknownErrorReportingMode mode ->
     [
       Text "Unknown error reporting mode: ";
       Code mode;
       Text ". Valid values are ";
       Code "plain";
       Text " and ";
       Code "json";
       Text "."
     ]
  | DuplicateFlags name ->
     [
       Text "The argument ";
       Code name;
       Text " was given multiple times"
     ]
  | EmptyArgs ->
     [
       Text "Argument list must have at least one element in it, the path to the binary. Was the compiler invoked through a system call?"
     ]
  | ExpectedBoolFlag name ->
     [
       Text "Expected ";
       Code name;
       Text " to be a boolean flag, but it was given as a value flag."
     ]
  | ExpectedValueFlag name ->
     [
       Text "Expected ";
       Code name;
       Text " to be a value flag, but it was given as a boolean flag."
     ]
  | UnknownFlagType flag ->
     [
       Text "Invalid flag argument ";
       Code flag
     ]
  | CCompilerError { command; exit_code; stdout; stderr } ->
     [
       Text "Failed to run C compiler.";
       Break;
       Text "Command: ";
       Code command;
       Break;
       Text "Exit code: ";
       Text (string_of_int exit_code);
       Break;
       Text "Standard output:\n";
       Text stdout;
       Break;
       Text "Standard error:\n";
       Text stderr
     ]
  | WriteError { path; reason } ->
     (match reason with
      | Some msg ->
         [
           Text "Failed to write to file ";
           Code path;
           Text ": ";
           Break;
           Text msg
         ]
      | None ->
         [
           Text "Failed to write to file ";
           Code path
         ])
