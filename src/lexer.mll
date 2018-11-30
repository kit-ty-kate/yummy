rule main = parse
  | eof { Parser.EOF }
  | '\n' { Parser.Newline }
  | ':' { Parser.Colon }
  | '-' { Parser.Hyphen }
  | ' '+ as level { Parser.Spaces (String.length level) }
  | [^ '\n' ':' ' ']+ as str { Parser.Word str }
  | _ { assert false (* NOTE: Cannot be reached *) }
