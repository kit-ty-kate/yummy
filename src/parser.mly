%token <int> Spaces
%token <string> Word
%token Colon
%token Hyphen
%token Newline
%token EOF

%start main
%type <ParseTree.t> main

%%

let main :=
  | x = line; xs = main; { x::xs }
  | level; EOF; { [] }

let line :=
  | lvl = level; k = Word; Colon; Spaces?; newline; { (lvl, ParseTree.Map (k, None)) }
  | lvl = level; k = Word; Colon; Spaces; str = value; newline; { (lvl, ParseTree.Map (k, Some str)) }
  | lvl = level; str = string; newline; { (lvl, ParseTree.String str) }
  | lvl = level; Hyphen; Spaces?; newline; { (lvl, ParseTree.ListElm None) }
  | lvl = level; Hyphen; Spaces; str = value; newline; { (lvl, ParseTree.ListElm (Some str)) }

let level :=
  | { 0 }
  | lvl = Spaces; { lvl }
  | Spaces; Newline; x = level; { x }

let string :=
  | w = Word; Spaces?; { w }
  | w = value_elm; ws = string; { w^ws }
  | w1 = Word; n = Spaces; w2 = value_elm; ws = string; { w1^String.make n ' '^w2^ws }

let value :=
  | w = value_elm; Spaces?; { w }
  | w = value_elm; ws = value; { w^ws }
  | w1 = value_elm; n = Spaces; w2 = value_elm; ws = value; { w1^String.make n ' '^w2^ws }

let value_elm ==
  | w = Word; { w }
  | Colon; { ":" }

let newline := Newline | EOF
