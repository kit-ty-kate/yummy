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
  | lvl = level; t = stmt; newline; { (lvl, t) }

let stmt :=
  | k = string_colon; t = stmt_value; { ParseTree.Map (k, t) }
  | str = string; { ParseTree.String str }
  | Hyphen; t = stmt_value; { ParseTree.ListElm t }

let stmt_value :=
  | Spaces?; { None }
  | Spaces; t = stmt; { Some t }

let level :=
  | { 0 }
  | lvl = Spaces; { lvl }
  | Spaces; Newline; x = level; { x }

let string_colon :=
  | w = Word; opt(Spaces); Colon; { w }
  | w = Word; ws = string_colon; { w^ws }
  | Colon; ws = string_colon; { ":"^ws }
  | w = Word; n = Spaces; ws = string_colon; { w^String.make n ' '^ws }

let string :=
  | w = Word; Spaces?; { w }
  | w = Word; ws = string; { w^ws }
  | Colon; ws = string; { ":"^ws }
  | w = Word; n = Spaces; ws = string; { w^String.make n ' '^ws }

let newline := Newline | EOF

let opt(X) ==
  | { None }
  | x = X; { Some x }
