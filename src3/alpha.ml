open Syntax

let find env i = if List.mem_assoc i env then List.assoc i env else i

let mem env i = List.mem_assoc i env

let rec g env = function
  | Int(i) -> Int(i)
  | Float(i) -> Float(i)
  | Str(i) -> Str(i)
  | Raise(i) -> Raise(i)
  | Bool(i) -> Bool(i)
  | Var(s) -> Var(find env s)
  | Unit -> Unit
  | Bin(e1,op,e2) -> Bin(g env e1,op, g env e2)
  | Pre(op,e1) -> Pre(op, g env e1)
  | App(e,es) -> App(g env e, List.map (g env) es)
  | If(e1,e2,e3) -> If(g env e1, g env e2, g env e3)
  | Let(s,e1,e2) ->
    let s' = if mem env s then genid s else s in
    Let(s',g env e1, g ((s,s')::env) e2)
  | LetRec(s, e1, e2) ->
    let s' = if mem env s then genid s else s in
    LetRec(s', g ((s,s')::env) e1, g ((s,s')::env) e2)
  | Fun(is,e) ->
    let iis = List.map(fun i -> (i,i)) is in
    Fun(is, g (iis @ env) e)  
  | Rec(ies) ->
    Rec(List.map(fun(i,e)->(i,g env e)) ies)
  | Get(e1,e2) -> Get(g env e1, g env e2)
  | Put(e1,e2,e3) -> Put(g env e1, g env e2, g env e3)
  | Match (_, _) | Tuple _| Array (_, _)| CApp (_, _) -> assert false

let f e = g [] e
