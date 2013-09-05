open Jslib

let () =
  let program = Parse5.parse_from_file Sys.argv.(1) in
  Pretty.print_program program
