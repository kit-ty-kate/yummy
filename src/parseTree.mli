type level = int list
type key = string

type value =
  | Map of key * string option
  | String of string
  | ListElm of string option

type t = (level * value) list
