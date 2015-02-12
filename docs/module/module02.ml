type e =
| Int of int
| Bin of e * string * e
| Var of string list
| Let of string * e * e
| Unit
| LetRec of string * string list * e * e
| Closure of env * string list * e
| App of e * e list
| NFun of (env * e list -> env * e)
| Open of string
| Module of env
| Mod of string * es
and env = (string * e) list
and es = e list
and ss = string list
[@@deriving show]

let _ =
  Format.printf "%s\n" (show_e (Bin(Int 1, "+", Int 2)))
