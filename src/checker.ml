exception Cannot_parse_remaining
exception Illegal_map_value

type value = [
  | `String of string
  | `A of value list
  | `O of (string * value) list
]

(* TODO: Allow ListElm but need spaces from colon *)
let rec concat_strings indent str = function
  | (i, ParseTree.String s)::xs when i >= indent -> concat_strings indent (str ^ " " ^ s) xs
  | l -> (`String str, l)

let rec check indent x l = match x, l with
  | ParseTree.String str, l -> concat_strings indent str l
  | ParseTree.Map (key, x), l -> let (obj, l) = add_to_object indent key x l in (`O obj, l)
  | ParseTree.ListElm x, l -> let (a, l) = add_to_list indent x l in (`A a, l)

and add_to_object indent key x l =
  let (x, l) = get_map_content indent x l in
  let (obj, l) = get_object indent l in
  ((key, x)::obj, l)

and get_map_content indent x l =
  let l = match x with
    | None -> l
    | Some (Map _ | ListElm _) -> raise Illegal_map_value
    | Some x -> (succ indent, x)::l
  in
  match l with
  | (i, (ParseTree.ListElm x))::l when i >= indent -> let (a, l) = add_to_list i x l in (`A a, l)
  | (i, (ParseTree.String _ as x))::l when i > indent -> check (succ indent) x l
  | (i, x)::l when i > indent -> check i x l
  | _::_ | [] -> (`String "", l) (* TODO: define null *)

and get_object indent = function
  | (i, ParseTree.Map (key, x))::l when i = indent -> add_to_object i key x l
  | (_::_ | []) as l -> ([], l)

and add_to_list indent x l =
  let (x, l) = get_listelm_content indent x l in
  let (xs, l) = get_list indent l in
  (x::xs, l)

and get_listelm_content indent x l =
  let l = match x with
    | None -> l
    | Some (ParseTree.String _ as x) -> (succ indent, x)::l
    | Some (ParseTree.(Map _ | ListElm _) as x) -> (indent + 2, x)::l (* TODO: replace +2 by +Spaces *)
  in
  match l with
  | (i, (ParseTree.String _ as x))::l when i > indent -> check (succ indent) x l
  | (i, x)::l when i > indent -> check i x l
  | _::_ | [] -> (`String "", l) (* TODO: define null *)

and get_list indent = function
  | (i, ParseTree.ListElm x)::l when i = indent -> add_to_list i x l
  | (_::_ | []) as l -> ([], l)

let check = function
  | [] -> `String ""
  | (i, x)::l -> match check i x l with
    | (v, []) -> v
    | (_, _::_) -> raise Cannot_parse_remaining
