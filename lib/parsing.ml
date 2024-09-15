let parse_expr1_string string =
  let lexbuf = Lexing.from_string string in
  let out = Parser.program1 Lexer.token lexbuf in
  out

let parse_expr2_string string =
  let lexbuf = Lexing.from_string string in
  let out = Parser.program2 Lexer.token lexbuf in
  out

