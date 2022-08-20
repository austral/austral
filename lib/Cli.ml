open CliUtil
open CliParser
open CliEngine
open Error

let rec main (args: string list): unit =
  let _ = Printexc.record_backtrace true in
  try
    main' args;
    exit 0
  with Austral_error error ->
    Printf.eprintf "%s" (render_error_to_plain error);
    print_endline ("Backtrace:\n" ^ (Printexc.get_backtrace ()));
    dump_and_die ()

and main' (args: string list): unit =
  let arglist: arglist = parse_args args in
  let cmd: cmd = parse arglist in
  exec cmd

and dump_and_die _: unit =
  print_endline "Compiler call tree printed to calltree.html";
  Reporter.dump ();
  exit (-1)
