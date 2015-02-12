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

module FileSystem = struct
  let files = [
    "test.ml",[
      Int 1;
      Int 2;
    ];
  ]
  let read f = List.assoc f files
end

let _ =
  let filename = "test.ml" in
  let es = FileSystem.read filename in
  Format.printf "%s=%s\n" filename (show_es es)
