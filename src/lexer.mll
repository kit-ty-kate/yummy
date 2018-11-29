rule main = parse
  | '\n' (' '* as level) { Parser.Level (String.length level) }
  | ':' ' '* { Parser.Colon }
  | '-' ' '* { Parser.Hyphen }
  | [^ '\n' ':']+ as str { Parser.Words str }
  | eof { Parser.EOF }
  | _ { assert false (* NOTE: Cannot be reached *) }
