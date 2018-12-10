type value = [
  | `String of string
  | `A of value list
  | `O of (string * value) list
]

val check : ParseTree.t -> value
