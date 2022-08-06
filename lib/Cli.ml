open CliUtil
open CliParser
open CliEngine
open Error

let rec main (args: string list): unit =
  try
    main' args;
    exit 0
  with Austral_error error ->
    Printf.eprintf "%s" (render_error_to_plain error);
    dump_and_die ()

and main' (args: string list): unit =
  let arglist: arglist = parse_args args in
  let cmd: cmd = parse arglist in
  exec cmd

and dump_and_die _: unit =
  print_endline "Compiler call tree printed to calltree.html";
  Reporter.dump ();
  exit (-1)
