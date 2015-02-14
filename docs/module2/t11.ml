module S = struct
  type s =
    | Unit
    | Var of string list
    | Mod of string * s * s
    | Open of string * s
    | Let of string * s * s
    | Int of int
    | Bin of s * string * s
end

type e =
  | Unit
  | Open of string * e
  | Mod of string * e * e
  | Var of string * e * e
and v =
  | VEnv of env * r
  | VCycle
and r =
  | RUnit
  | RCycle
  | RRef of r
  | RLet of string * r * r
and env = {g:(string * v) list;l:(string * v) list}
and ss = string list
and sss = (string * string list) list
[@@deriving show]

module FS1 = struct
  open S

  let files = [
    "a.ml",
      Let("a", Int 1,
      Unit);
    "b.ml",
      Let("b", Var["A";"a"],
      Unit);
    "c.ml",
      Open("B",
        Bin(Var["A";"a"],"+",Var ["b"])
      );
  ]
  let read f = List.assoc f files
end

module FS = struct

  let filename f =
    (String.uncapitalize f) ^ ".ml"

  let modulename f =
    let m = (String.capitalize f) in
    String.sub m 0 ((String.length m) - 3)

  let rec convert r = function
    | S.Unit -> r
    | S.Var(x::_::_) -> Var(x,Unit,r)
    | S.Var(_) -> r
    | S.Mod(x,s1,s2) -> Mod(x, convert Unit s1, convert r s2)
    | S.Open(x,s) -> Open(x, convert r s)
    | S.Let(x,s1,s2) -> convert (convert r s2) s1
    | S.Int(_) -> r
    | S.Bin(s1,_,s2) -> convert (convert r s2) s1

  let read f =
    let s = FS1.read(filename f)in
    let e = convert Unit s in
    e
end



let rcons = function
  | (RUnit,r) | (r,RUnit) -> r
  | (r1,r2) -> RLet("_",r1, r2)

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
    | RLet("_",r1,r2) -> sort r1 @ sort r2
    | RLet(s,r1,r2) -> sort r1 @ s :: sort r2
  in nub (sort e)

let empty = {g=[];l=[]}
let rec occur {g=env} x =
  List.mem_assoc x env

let rec loccur {l=env} x =
  List.mem_assoc x env

let rec get_cache {g=env} x = 
  match List.assoc x env with
  | VEnv(e,r) -> (e,r)
  | VCycle -> (empty,RCycle)

let rec get_lcache {l=env} x = 
  match List.assoc x env with
  | VEnv(e,r) -> (e,r)
  | VCycle -> (empty,RCycle)

let add_cache {g=g;l=l} x v = {g=(x,v)::g;l=l}
let add_lcache {g=g;l=l} x v = {g=g;l=(x,v)::l}

let rec eval (env:env) = function
  | Unit -> (env, RUnit)
  | Open(x1, e2) ->
    if loccur env x1 then
      let (env, r1) = lcache env x1 in
      let (env, r2) = eval env e2 in
      (env, r2)
    else if occur env x1 then
      let (env, r1) = cache env x1 in
      let (env, r2) = eval env e2 in
      (env, RLet(x1, r1, r2))
    else
      let (env, r1) = read env x1 in
      let (env, r2) = eval env e2 in
      (env, RLet(x1, r1, r2))
  | Mod(s, e1, e2) ->
    let (env1, r1) = eval env e1 in
    let env = {g=env1.g @ env.g;l=env.l} in
    let (env, r2) = eval (add_lcache env s (VEnv(env1, r1))) e2 in
    (env, rcons(r1, r2))
  | Var(x0, e1, e2) ->
    if loccur env x0 then
      let (env1, r0) = lcache env x0 in
      let (env1, r1) = eval env1 e1 in
      let (env, r2) = eval (add_lcache env x0 (VEnv(env1, r1))) e2 in
      (env, rcons(r0, rcons(r1,r2)))
    else if occur env x0 then
      let (env1, r0) = cache env x0 in
      let (env1, r1) = eval (add_cache env x0 (VEnv(env1, r0))) e2 in
      let (env, r2) = eval (add_cache env x0 (VEnv(env1, r1))) e2 in
      (env, RLet(x0, r0, rcons(r1, r2)))
    else
      let (env1, r0) = read env x0 in
      let (env1, r1) = eval env1 e1 in
      let (env, r2) = eval (add_cache env x0 (VEnv(env1, r1))) e2 in
      (env, RLet(x0, r0, rcons(r1, r2)))
and read {g=g;l=l} x1 =
  (*
  Format.printf "read %s\n" x1;
  *)
  let ({g=g1;l=l1}, r1) = eval (add_cache {g=g;l=[]} x1 VCycle) (FS.read x1) in
  (add_cache {g=g1;l=l1@l} x1 (VEnv ({g=g1;l=l1}, r1)), r1)
and cache env x1 =
  (*
  Format.printf "cache %s\n" x1;
  *)
  let (env1, r1) = get_cache env x1 in
  (env, RRef r1)
and lcache {g=g;l=l} x1 =
  (*
  Format.printf "lcache %s\n" x1;
  *)
  let ({g=g1;l=l1}, r1) = get_lcache {g=g;l=l} x1 in
  (add_lcache {g=g;l=l1@l} x1 (VEnv ({g=g1;l=l1}, r1)), RRef r1)
and start x =
  let (_,r) = read empty x in
  RLet(x, r, RUnit)

let analize ml =
  let x = FS.modulename ml in
  let r = start x in
  sort r

let _ =
  let r = analize "c.ml" in
  Printf.printf "result %s\n" (show_ss r)
