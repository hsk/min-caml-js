type e =
  | Unit
  | Let of string * e * e
and l =
  | Tag of string * l list
and ls = l list
and sss = (string * string list) list
[@@deriving show]

let e =
  Let("O1",
    Let("N1",
      Let("M1", Unit,
      Let("M2", Unit,
      Unit)),
    Let("N2",
      Let("M1", Unit,
      Let("M2", Unit,
      Unit)),
    Unit)),
  Let("O2",
    Let("N1",
      Let("M1", Unit,
      Let("M2", Unit,
      Unit)),
    Let("N2",
      Let("M1", Unit,
      Let("M2", Unit,
      Unit)),
    Unit)),
  Unit))

let l =
  Tag("O1",[
    Tag("N1", [
      Tag("M1", []);
      Tag("M2", []);
    ]);
    Tag("N1", [
      Tag("M1", []);
      Tag("M2", []);
    ]);
  ])

let rec f (e:e):l list =
  match e with
  | Unit -> []
  | Let(x,e1,e2) ->
     Tag(x, f e1)::(f e2)

let rec f2 = function
  | Tag(s,ls) ->
    let tagnames = List.map(fun (Tag(n,_)) -> n) ls in
    (s,tagnames)::(List.fold_left (fun l tag -> (f2 tag)@l) [] ls)

let rec f3 (e:e) =
  match e with
  | Unit -> []
  | Let(x,e1,e2) ->

    let rec names (e:e):string list =
      match e with
      | Unit -> []
      | Let(x,_,e2) ->
         x::(names e2)
    in
    let names = names e1 in
    (x,names)::(f3 e1)@(f3 e2)

let rec f4 env e =
  match e with
  | Unit -> env
  | Let(x,e1,e2) ->
    let rec names = function
      | Unit -> []
      | Let(x,_,e2) -> x::(names e2)
    in
    if(List.mem_assoc x env) then
      (f4 (f4 env e2) e1)
    else
      (f4 (f4 ((x,names e1)::env) e2) e1)

let _ =
  let l = f e in
  Printf.printf "%s\n" (show_ls l);
  let sss = f2 (Tag("root",l)) in
  Printf.printf "f2 %s\n" (show_sss sss);

  let sss = f3 e in
  Printf.printf "f3 %s\n" (show_sss sss);

  let sss = f4 [] e in
  Printf.printf "f4 %s\n" (show_sss sss)
