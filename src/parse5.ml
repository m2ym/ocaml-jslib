let parse rule lexbuf =
  rule (Lexer.token ()) lexbuf

let parse_from_string rule string =
  parse rule (Lexing.from_string string)

let parse_from_channel rule channel =
  parse rule (Lexing.from_channel channel)

let parse_from_file rule file =
  let c = open_in file in
  try
    let ast = parse_from_channel rule c in
    close_in c;
    ast
  with e ->
    close_in c;
    raise e

let parse_expression              = parse Parser5.parse_expression
let parse_expression_from_string  = parse_from_string Parser5.parse_expression
let parse_expression_from_channel = parse_from_channel Parser5.parse_expression
let parse_statement               = parse Parser5.parse_statement
let parse_statement_from_string   = parse_from_string Parser5.parse_statement
let parse_statement_from_channel  = parse_from_channel Parser5.parse_statement
let parse_program                 = parse Parser5.parse_program
let parse_program_from_string     = parse_from_string Parser5.parse_program
let parse_program_from_channel    = parse_from_channel Parser5.parse_program
