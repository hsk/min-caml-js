type e =
| Unit
| Open of string * e
| Mod of string * e * e
and v =
| Env of env 
and r =
| RUnit
| Result of string * r * r
| RCons of r * r
and env = (string * v) list
and ss = string list
[@@deriving show]

module FileSystem = struct
  let files = [
    "A",
      Unit;
    "E",
      Mod ("Inner",
        Mod ("Inner2",Open("A",Unit),
      Open("Inner",
      Open("Inner2",
      Unit
      ))),Unit);
    "F", 
      Open ("E", Unit);
    "Inner", Unit;
    "G",
      Open("Inner", Open ("E", Open ("F", Unit)));
    "H", Open("I", Unit);
    "I", Open("H", Unit);

    "J", Open("K", Unit);
    "K", Open("L", Unit);
    "L", Open("J", Unit);

  ]
  let read f = List.assoc f files
end

let rcons = function
  | (RUnit,r) | (r,RUnit) -> r
  | (r1,r2) -> RCons(r1, r2)

let rec sort = function
  | RUnit -> []
  | Result(s,r1,r2) -> sort r1 @ s :: sort r2
  | RCons(r1,r2) -> sort r1 @ sort r2

let rec eval (env:env) = function
  | Unit -> (env, RUnit)
  | Open(x1, e2) ->
    if List.mem_assoc x1 env then (
      let Env env1 = (List.assoc x1 env) in
      eval (env1 @ env) e2
    ) else (
      let (env, r1) = read env x1 in
      let (env, r2) = eval ((x1, Env env)::env) e2 in
      (env, Result(x1, r1, r2))
    )
  | Mod(s, e1, e2) ->
    let (env, r1) = eval env e1 in
    let (env, r2) = eval ((s,Env [])::env) e2 in
    (env, rcons(r1, r2))
and read env x1 =

    Format.printf "read %s\n" x1;
    let (env1, r1) = eval ((x1,Env [])::env) (FileSystem.read x1) in
    (env1@env, r1)
and start x =
  let (env,r) = read [] x in
  Result(x, r, RUnit)

let _ =
  let r = start "G" in
  Format.printf "%s\n" (show_r r);

  let ss = sort r in
  Format.printf "%s\n" (show_ss ss);

  let r = start "H" in
  Format.printf "%s\n" (show_r r);

  let ss = sort r in
  Format.printf "%s\n" (show_ss ss);

  let r = start "J" in
  Format.printf "%s\n" (show_r r);

  let ss = sort r in
  Format.printf "%s\n" (show_ss ss);
  
