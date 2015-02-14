type e =
| Unit
| Open of string * e
| Mod of string * e * e
and v =
| Env of env 
and env = (string * v) list
and ss = string list
[@@deriving show]

module FileSystem = struct
  let files = [
    "A",
      Unit;
    "E",
      Mod ("Inner",
        Mod ("Inner2",Unit,Unit),
      Open("A",
      Open("Inner",
      Open("Inner2",
      Unit
      ))));
    "F", 
      Open ("E", Unit);
  ]
  let read f = List.assoc f files
end

let rec eval (env:env) = function
  | Unit -> (env, [])
  | Open(x1, e2) ->
    if List.mem_assoc x1 env then (
      let Env env1 = (List.assoc x1 env) in
      eval (env1 @ env) e2
    ) else (
      Format.printf "read %s\n" x1;
      let (env, r1) = eval env (FileSystem.read x1) in
      let (env, r2) = eval ((x1, Env env)::env) e2 in
      (env, x1 :: r1 @ r2)
    )
  | Mod(s, e1, e2) ->
    let (env, r1) = eval env e1 in
    let (env, r2) = eval ((s,Env env)::env) e2 in
    (env, r1 @ r2)

let _ =
  let (env, r) = eval [] (FileSystem.read "F") in
  Printf.printf "%s\n" (show_ss r)


