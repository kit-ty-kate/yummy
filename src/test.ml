let f x =
  Checker.check (Parser.main Lexer.main (Lexing.from_string x))

let f' x =
  let x = open_in x in
  let x = Lexing.from_channel x in
  Checker.check (Parser.main Lexer.main x)

type line =
  | Empty
  | List of line
  | Map of string * line
  | String of string

let cl_to_string cl =
  let len = List.length cl in
  let b = Bytes.create len in
  List.iteri (Bytes.set b) cl;
  Bytes.to_string b

let concat_cls cl s =
  cl_to_string cl ^ s

module Parser = struct
  open Angstrom

  module Tokens = struct
    let is_space = function ' ' -> true | _ -> false
    let is_colon = function ':' -> true | _ -> false
    let colon_space = string ": " *> return ()
    let space = char ' ' *> return ()
    let eol = choice [end_of_line; end_of_input]
  end

  module Handles = struct
    let line spaces str =
      (String.length spaces, str)

    let empty _ = Empty
    let empty_list _ = List Empty
    let list line = List line
    let empty_map name = Map (name, Empty)
    let map name line = Map (cl_to_string name, line)
    let string str = String str
  end

  let trimmed_string =
    fix begin fun trimmed_string ->
      choice [
        (^) <$> begin
          concat_cls <$> many_till any_char Tokens.space <*> return " "
        end <*>
        choice [
          (^) <$> take_while Tokens.is_space <*> trimmed_string;
          take_while Tokens.is_space *> return "";
        ];
        concat_cls <$> many_till any_char Tokens.eol <*> return "";
      ]
    end

  let line_content =
    fix begin fun line_content ->
      choice [
        Handles.list <$> string "- " *> line_content;
        Handles.map <$> many_till any_char Tokens.colon_space <*> line_content; (* any_char is wrong *)
        Handles.empty_map <$> take_till Tokens.is_colon <* char ':';
        Handles.empty_list <$> char '-';
        Handles.string <$> trimmed_string;
        Handles.empty <$> take_while Tokens.is_space;
      ]
    end

  let line =
    Handles.line <$>
    take_while Tokens.is_space <*>
    line_content <*
    Tokens.eol

  let lines =
    many_till line end_of_input

  let main =
    lines <* end_of_input
end

let g x =
  Angstrom.parse_string Parser.main x
