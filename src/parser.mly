%token <int> Level
%token <string> Words
%token Colon
%token Hyphen
%token EOF

%start main
%type <ParseTree.t> main

%%

let main :=
  | lvl = Level*; x = value; xs = line*; EOF; { (lvl, x)::xs }
  | Level*; EOF; { [] }

let value :=
  | key = Words; Colon; str = Words?; { ParseTree.Map (key, str) }
  | str = Words; { ParseTree.String str }
  | Hyphen; str = Words?; { ParseTree.ListElm str }

let line :=
  | lvl = Level+; x = value; { (lvl, x) }
