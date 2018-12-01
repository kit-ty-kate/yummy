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
  | k = string(Colon); t = stmt_value; { ParseTree.Map (k, t) }
  | str = string(empty); { ParseTree.String str }
  | Hyphen; t = stmt_value; { ParseTree.ListElm t }

let stmt_value :=
  | Spaces?; { None }
  | Spaces; t = stmt; { Some t }

let level :=
  | { 0 }
  | lvl = Spaces; { lvl }
  | Spaces?; Newline; x = level; { x }

let string(final) :=
  | w = Word; opt(Spaces); final; { w }
  | w = Word; ws = string(final); { w^ws }
  | Colon; ws = string(final); { ":"^ws }
  | w = Word; n = Spaces; ws = string(final); { w^String.make n ' '^ws }

let newline := Newline | EOF

let opt(X) ==
  | { None }
  | x = X; { Some x }

let empty := <>
