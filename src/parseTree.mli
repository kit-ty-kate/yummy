type level = int
type key = string

type value =
  | Map of key * value option
  | String of string
  | ListElm of value option

type t = (level * value) list
