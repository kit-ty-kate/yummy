let f x =
  let filebuf = Lexing.from_string x in
  Parser.main Lexer.main filebuf
