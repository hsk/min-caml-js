open Syntax

let rec f e = match e with
  | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
  | Pre (op, e1) -> Pre(op, f e1) 
  | Fun(ss, e) -> Fun(ss, f e)
  | Rec ses -> let rec f1 e = match e with (s,e) -> (s, f e) in Rec(List.map f1 ses)
  | App(e1,e2) -> App(f e1, List.map f e2)
  | Bin(e1,op,e2) -> Bin(f e1, op, f e2) 
  | Let(s,e1,e2) -> Let(s, f e1, f e2)
  | LetRec(s,e1,e2) -> LetRec(s, f e1, f e2)
  | If(e1,e2,e3) -> If(f e1, f e2, f e3) 
  | Get(e1,e2) -> Get(f e1, f e2) 
  | Put(e1,e2,e3) -> Put(f e1, f e2, f e3) 
  | Open(s,e) -> Open(s, f e)
  | Tuple(es) -> let rec f1 i n = ("_"^string_of_int i, n) in f (Rec(List.mapi f1 es))
  | Array (e1, e2) -> App(Var "Array.create", [f e1; f e2]) 
  | CApp(e1,e2) -> Rec([("tag", Str e1); ("data",f e2)] )
  | Match(e1,ss) ->
    let rec mat e me =
      match (e,me) with
      | (envs, conds), (Bin(m,"as",Var p), e) -> mat ((p,e)::envs, conds) (m, e)
      | (envs, conds), (Var p, e) -> ((p,e)::envs, conds)
      | (envs, conds), (Rec ms, e) ->
        let f1 env iv = match iv with
          | (i,v) -> mat env (v, Get(e,Str i))
        in
        List.fold_left f1 (envs, conds) ms
      | (envs, conds), (m, e) ->
        (envs, Bin(e, "==", m) :: conds)
    in
    let f1 mwf str =
      let (m, w, f1) = mwf in
      let (m, f1) = (f m, f f1) in
      let (envs,conds) = mat ([],[]) (m, Var "_") in
      let envs = List.rev envs in
      let conds = match w with
        | None -> conds
        | Some(e) ->
          App(Fun(List.map fst envs, f e), List.map snd envs) :: conds
      in 
      let ret = App(Fun(List.map fst envs,f1), List.map snd envs) in
      if conds = [] then
        ret
      else
        let f1 tl e = Bin(e, "&&", tl) in
        let cond = List.fold_left f1 (List.hd conds) (List.tl conds) in
        If(cond, ret, str)
    in
    let r = List.fold_right f1 ss (Raise "error") in
    App(Fun(["_"], r), [f e1])


let rec addtail t = function
  | Unit -> t
  | If(t1,t2,Unit) -> If(t1,t2, t)
  | If(t1,t2,t3) -> If(t1,t2, addtail t t3)
  | Match(t,ts) ->
    let rec f = function
      | [] -> (Var "_", None, t)::[]
      | (Var "_", None, e)::xs-> 
        (Var "_", None, addtail t e)::xs
      | t::xs -> t::(f xs)
    in
    Match(t, f ts)
  | t -> Format.printf "%a" Syntax.show_e t; assert false

let rec if2if e =
  match e with
  | Bin(e1,"&&",e2) -> 

    if2if (If(if2if e1, if2if e2, Int 0) )


  | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
  | Pre (op, e1) -> Pre(op, if2if e1) 
  | Fun(ss, e) -> Fun(ss, if2if e)
  | Rec ses -> let rec f1 e = match e with (s,e) -> (s, if2if e) in Rec(List.map f1 ses)
  | App(e1,e2) -> App(if2if e1, List.map if2if e2)
  | Bin(e1,op,e2) -> Bin(if2if e1, op, if2if e2) 
  | Let(s,e1,e2) -> Let(s, if2if e1, if2if e2)
  | LetRec(s,e1,e2) -> LetRec(s, if2if e1, if2if e2)
  | Get(e1,e2) -> Get(if2if e1, if2if e2) 
  | Put(e1,e2,e3) -> Put(if2if e1, if2if e2, if2if e3) 
  | Open(s,e) -> Open(s, if2if e)


  | If(t1, If(t11,t12, t13), If(t21,t22,t23)) when t1 = t21 ->
    let If(t21,t22,t23) = if2if (If(t21,t22,t23)) in
    (If(t1, If(t11, t12, addtail t13 t22), t23))
  | If(t1,t2,t3) -> If(t1, t2, if2if t3)
  | Match _ -> assert false
  | Tuple _ | Array _ | CApp _ -> assert false

let rec if2Match e = 
  match e with
  | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
  | Pre (op, e1) -> Pre(op, if2Match e1) 
  | Fun(ss, e) -> Fun(ss, if2Match e)
  | Rec ses -> let rec f1 e = match e with (s,e) -> (s, if2Match e) in Rec(List.map f1 ses)
  | App(e1,e2) -> App(if2Match e1, List.map if2Match e2)
  | Bin(e1,op,e2) -> Bin(if2Match e1, op, if2Match e2) 
  | Let(s,e1,e2) -> Let(s, if2Match e1, if2Match e2)
  | LetRec(s,e1,e2) -> LetRec(s, if2Match e1, if2Match e2)
  | Get(e1,e2) -> Get(if2Match e1, if2Match e2) 
  | Put(e1,e2,e3) -> Put(if2Match e1, if2Match e2, if2Match e3) 
  | Open(s,e) -> Open(s, if2Match e)
  | If(Bin(t11,"==",t12), t13, (If(Bin(t21,"==",t22),_,_) as t2)) when t11 = t21 ->
    begin match if2Match t2 with
    | Match(t2,ls) when t11 = t2 -> Match(t2,(t12,None,if2Match t13)::ls) 
    | If(Bin(t21,"==",t22), t23, t24) when t11 = t21 -> Match(t21,[(t12,None,if2Match t13);(t22,None,t23);Var "_",None,t24])
    | _ -> If(Bin(t11,"==",t12), if2Match t13, if2Match t2)
    end
  | If(t1,t2,t3) -> If(t1, if2Match t2, if2Match t3)
  | Match(a,b) -> Match (a,b)

let f e = if2Match(if2if (f e))
