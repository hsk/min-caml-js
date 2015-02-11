open Syntax

(*
let threshod = ref 1000

let find (env:(string * (string list * e)) list ) i = List.assoc i env

let mem (env:(string * (string list * e)) list ) i = List.mem_assoc i env

let rec size = function
  | Get(e1,e2) | Bin(e1,_,e2) | Let(_,e1,e2) | LetRec(_, e1, e2) ->
    1 + (size e1) + (size e2)
  | Pre(_,e1) | Fun(_,e1) ->
    1 + size e1
  | If(e1,e2,e3) | Put(e1,e2,e3) ->
    1 + size e1 + size e2 + size e3
  | App(e,es) -> List.fold_left (+) (1 + size e) (List.map size es) 
  | Rec(ies) -> List.fold_left (+) 1 (List.map(fun(_,e)->size e) ies)
  | _ -> 1
*)
let inline env (zs,e) (ys:e list) =
      let (zs,e) = List.fold_right2 (fun z y (zs, e) ->
        match y with
        | Var n -> ((z,n)::zs, e)
        | _ -> let i = gentmp() in ((z,i)::zs, Let(i, y, e))
      ) zs ys ([],e) in

      Alpha.g zs e

let rec g env = function
  | Match (_, _) | Tuple _| Array (_, _)| CApp (_, _) -> assert false
  | Int(i) -> Int(i)
  | Float(i) -> Float(i)
  | Str(i) -> Str(i)
  | Raise(i) -> Raise(i)
  | Bool(i) -> Bool(i)
  | Unit -> Unit
  | Bin(e1,op,e2) -> Bin(g env e1,op, g env e2)
  | Pre(op,e1) -> Pre(op, g env e1)
  | If(e1,e2,e3) -> If(g env e1, g env e2, g env e3)
  | Rec(ies) -> Rec(List.map(fun(i,e)->(i,g env e)) ies)
  | Get(e1,e2) -> Get(g env e1, g env e2)
  | Put(e1,e2,e3) -> Put(g env e1, g env e2, g env e3)
  | Var(s) -> Var(s)
  | Fun(is,e) -> Fun(is, g env e)
(*

  | Let(s,Fun(is,e1),e2) ->
    let env = if size e1 > !threshod then env else (s,(is,e1))::env in
    Let(s, Fun(is, g env e1), g env e2)
  | LetRec(s, Fun(is,e1), e2) ->
    let env = if size e1 > !threshod then env else (s,(is,e1))::env in
    LetRec(s, Fun(is, g env e1), g env e2)
  | App(Var s, es) when mem env s ->
    let f = find env s in
    Format.eprintf "inlining %s@." s;
    inline env f es*)
  | App(Fun(is,e1), es) ->
    Format.eprintf "inlining anonymous@.";
    let f = (is, g env e1) in
    inline env f es

  | Let(s,e1,e2) ->
    Let(s,g env e1, g env e2)
  | LetRec(s, e1, e2) ->
    LetRec(s, g env e1, g env e2)
  | App(e,es) -> App(g env e, List.map (g env) es)


let f e = g [] e
(*let f e = e*)
