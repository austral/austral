let split_common_path (path: string): string * string =
  (path ^ ".aui", path ^ ".aub")

let main (args: string list): unit =
  let _ = List.map split_common_path args in
  ()
