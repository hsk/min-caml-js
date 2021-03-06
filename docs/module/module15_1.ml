module Set = struct
  let uniq x =
    let rec f l n = 
      match l with
      | [] -> []
      | h :: t -> if n = h then f t n else h::(f t n)
    in
    match x with
    | [] -> []
    | h::t -> f t h

  let rec rem (elt : 'a) (lst : 'a list) : 'a list = match lst with
      [] -> []
    | x :: xs -> if elt = x then rem elt xs else x :: (rem elt xs)

  let rec nub (lst : 'a list) : 'a list = match lst with
      [] -> []
    | x :: xs -> x :: (nub (rem x xs))
end

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

let rec eval = function
  | Unit -> []
  | Open(x,e) -> x::(eval e)
  | Mod(x,e,e2) -> x::(eval e)@(eval e2)

let rec evalf = function
  | FUnit -> []
  | FFile(x,e,f) -> x::(eval e) @ (evalf f)

let _ =
  Printf.printf "%s\n" (show_ss (evalf files))

let rec eval = function
  | Unit -> []
  | Open(x,e) -> x::(eval e)
  | Mod(x,e,e2) -> x::(eval e)@(eval e2)

let rec evalf = function
  | FUnit -> []
  | FFile(x,e,f) -> (x,(eval e)) :: (evalf f)

let _ =
  Printf.printf "%s\n" (show_sss (evalf files))


let rec eval = function
  | Unit -> []
  | Open(x,e) -> x::(eval e)
  | Mod(x,e,e2) -> (eval e)@(eval e2)

let rec evalf = function
  | FUnit -> []
  | FFile(x,e,f) -> (x,(eval e)) :: (evalf f)

let _ =
  Printf.printf "%s\n" (show_sss (evalf files))


let rec read (file:string) = function
  | FUnit -> failwith ("NOT FOUND " ^ file)
  | FFile((x:string),(e:e),_)
    when x = file ->
    e
  | FFile(_,_,xs) -> read file xs

let rec eval files = function
  | Unit -> []
  | Open(x,e) -> x::(eval files e)
  | Mod(x,e,e2) -> (eval files e)@(eval files e2)

let rec evalf files file =
  let e = read file files in
  eval files e

let _ =
  Printf.printf "%s\n" (show_ss (evalf files "H"))

(* 
openしたらenvにいれよう。
 *)
let rec eval files (env:(string * string list)list) = function
  | Unit -> (env,[])
  | Open(x,e) ->
    begin try
      let m = List.assoc x env in
      eval files ((x,m)@env) e
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

