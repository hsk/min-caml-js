open Syntax

let rec f e = match e with
  | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
  | Pre (op, e1) -> Pre(op, f e1) 
  | Array (e1, e2) -> App(Var "makeArray", [f e1; f e2]) 
  | Fun(ss, e) -> Fun(ss, f e)
  | Rec ses -> let rec f1 e = match e with (s,e) -> (s, f e) in Rec(List.map f1 ses)
  | App(e1,e2) -> App(f e1, List.map f e2)
  | Bin(e1,op,e2) -> Bin(f e1, op, f e2) 
  | Let(s,e1,e2) -> Let(s, f e1, f e2)
  | LetRec(s,e1,e2) -> LetRec(s, f e1, f e2)
  | If(e1,e2,e3) -> If(f e1, f e2, f e3) 
  | Tuple(es) -> let rec f1 i n = ("_"^string_of_int i, n) in f (Rec(List.mapi f1 es))
  | Get(e1,e2) -> Get(f e1, f e2) 
  | Put(e1,e2,e3) -> Put(f e1, f e2, f e3) 
  | CApp(e1,e2) -> Rec([("tag", Str e1); ("data",f e2)] )
  | Match(e1,ss) ->
    let rec mat e me =
      match (e,me) with
      | (envs, conds), (Bin(m,"as",Var p), e) -> mat ((p,e)::envs, conds) (m, e)
      | (envs, conds), (Var p, e) -> ((p,e)::envs, conds)
      | (envs, conds), (Rec ms, e) ->
        let f1 env iv = match iv with
          | (i,v) -> mat env (v, Bin(e,".",Var i))
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
