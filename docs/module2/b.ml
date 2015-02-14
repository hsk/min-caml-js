type e =
  | Unit
  | Open of string * e
and v =
  | VEnv of env * r
  | VCycle
and r =
  | RUnit
  | RCycle
  | RRef of r
  | RLet of string * r * r
and env = {g:(string * v) list}
and ss = string list
[@@deriving show]

module FS = struct
  let files = [
    "A",
      Unit;
    "E",
      Open("A", Unit);
    "F", 
      Open("E", Unit);
    "Inner",
      Open("F", Unit);
    "G",
      Open("Inner", Open("E", Open("F", Unit)));
    "H", Open("I", Unit);
    "I", Open("H", Unit);
    "J", Open("K", Unit);
    "K", Open("L", Unit);
    "L", Open("J", Unit);

  ]
  let read f = List.assoc f files
end

let empty = {g=[]}
let rec occur {g=env} x =
  List.mem_assoc x env

let rec get_cache {g=env} x = 
  match List.assoc x env with
  | VEnv(e,r) -> (e,r)
  | VCycle -> (empty,RCycle)

let add_cache {g=env} x v = {g=(x,v)::env}

let rec eval (env:env) = function
  | Unit -> (env, RUnit)
  | Open(x1, e2) ->
    if occur env x1 then
      let (env, r1) = cache env x1 in
      let (env, r2) = eval env e2 in
      (env, RLet(x1, r1, r2))
    else
      let (env, r1) = read env x1 in
      let (env, r2) = eval env e2 in
      (env, RLet(x1, r1, r2))
and read env x1 =
  Format.printf "read %s\n" x1;
  let (env1, r1) = eval (add_cache env x1 VCycle) (FS.read x1) in
  (add_cache env1 x1 (VEnv (env1, r1)), r1)
and cache env x1 =
  Format.printf "cache %s\n" x1;
  let (env1, r1) = get_cache env x1 in
  (env, RRef r1)
and start x =
  let (_,r) = read empty x in
  RLet(x, r, RUnit)

let sort e =
  let rec rem (elt : 'a) (lst : 'a list) : 'a list = match lst with
    | [] -> []
    | x :: xs -> if elt = x then rem elt xs else x :: (rem elt xs)
  in
  let rec nub (lst : 'a list) : 'a list = match lst with
    | [] -> []
    | x :: xs -> x :: (nub (rem x xs))
  in
  let rec sort = function
    | RUnit -> []
    | RCycle -> []
    | RRef _ -> []
    | RLet(s,r1,r2) -> sort r1 @ s :: sort r2
  in nub (sort e)

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
