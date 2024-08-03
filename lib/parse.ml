open Error

let parse_expr s =
  (* let _ = Parsing.set_trace true in *)
  let lexbuf = Lexing.from_string s in
  Parser.expr Lexer.token lexbuf

let parse_stmt s =
  (* let _ = Parsing.set_trace true in *)
  let lexbuf = Lexing.from_string s in
  Parser.stmt Lexer.token lexbuf

let parse s =
  (* let _ = Parsing.set_trace true in *)
  let lexbuf = Lexing.from_string s in
  Parser.compile_unit Lexer.token lexbuf
