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
      Bin (Int 1,"+", Int 2);
    ];
  ]
  let read f = List.assoc f files
end

let rec eval (env:(string * e)list) = function
  | Int i -> (env, Int i)
  | Bin(a,"+",b) ->
    let (env,a) = eval env a in
    let (env,b) = eval env b in
    begin match (a,b) with
    | Int a, Int b -> (env, Int(a + b))
    | _ -> assert false
    end
  | e ->
    assert false

let rec evals env = function
  | [] -> env
  | x::xs ->
    Format.printf "eval : %s\n" (show_e x);
    let (env, x) = eval env x in
    Format.printf "- : %s\n" (show_e x);
    evals env xs

let _ =
  let env = [] in
  let env = evals env (FileSystem.read "test.ml") in
  env
