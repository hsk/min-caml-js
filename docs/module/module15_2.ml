type e =
  | Unit
  | Open of string * e
  | Mod of string * e * e
and f =
  | FUnit
  | FFile of string * e * f
and ss = string list
and sss = (string * string list) list
[@@deriving show]

let files =
  FFile("G", Mod("B",Mod("C",Unit,Unit),Unit),
  FFile("H", Open("G",Open("B",Open("C",Unit))),
  FUnit))

let rec read (file:string) = function
  | FUnit -> failwith ("NOT FOUND " ^ file)
  | FFile((x:string),(e:e),_)
    when x = file ->
    e
  | FFile(_,_,xs) -> read file xs

let rec eval files (env:(string * string list)list) = function
  | Unit -> (env,[])
  | Open(x,e) ->
    begin try
      let _ = List.assoc x env in
      eval files env e
    with _ ->
      let (env2, r) = evalf files x in
      eval files (env2@env) e
    end
  | Mod(x,e1,e2) ->
    let (env, e1) = eval files env e1 in
    let (env, e2) = eval files env e2 in
    (env, e1 @ e2)
and evalf files file =
  let e = read file files in
  let (env,e) = eval files [] e in
  ((file,e)::env, e)
  
let _ =
  let (env,e) = (evalf files "H") in
  Printf.printf "%s\n" (show_ss e)

