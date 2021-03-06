type e =
  | Unit
  | Open of string * e
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

    "M1",Unit;
    "M2",Unit;
    "M3",Unit;
    "N1",
      Open("M1",Open("M2",Open("M3",Unit)));
    "N2",
      Open("M1",Open("M2",Open("M3",Unit)));
    "N3",
      Open("M1",Open("M2",Open("M3",Unit)));
    "O1",
      Open("N1",Open("N2",Open("N3",Unit)));
    "O2",
      Open("N1",Open("N2",Open("N3",Unit)));
    "O3",
      Open("N1",Open("N2",Open("N3",Unit)));
    "P1",
      Open("O1",Open("O2",Open("O3",Unit)));

  ]
  let read f = List.assoc f files
end

type r =
  | RUnit
  | RLet of string * r * r
  | RRef of r
  | RCycle
and ss = string list
[@@deriving show]


type v =
  | VEnv of env * r
  | VCycle
and env = {g:(string * v) list}
[@@deriving show]

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
  let (env1, r1) = eval (add_cache env x1 VCycle) (FS.read x1) in
  (add_cache env1 x1 (VEnv (env1, r1)), r1)
and cache env x1 =
  let (env1, r1) = get_cache env x1 in
  (env, RRef r1)
and start x:r =
  let (_,r) = read empty x in
  RLet(x, r, RUnit)

let test file =
  Format.printf "## %s\n" file;
  let r = start file in
  Format.printf "    %s\n" (show_r r)

let _ =
  List.iter(fun (file,_) ->
    test file
  ) FS.files
