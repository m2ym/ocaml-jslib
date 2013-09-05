let parse_from_lexbuf lexbuf =
  Parser5.program (Lexer.token ()) lexbuf

let parse_from_string s =
  parse_from_lexbuf (Lexing.from_string s)

let parse_from_channel c =
  parse_from_lexbuf (Lexing.from_channel c)

let parse_from_file file =
  let c = open_in file in
  try
    let ast = parse_from_channel c in
    close_in c;
    ast
  with e ->
    close_in c;
    raise e
