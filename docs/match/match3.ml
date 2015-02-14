open Syntax
let rec show_e = function
  | Unit -> "undefined"
  | Var s -> s
  | Str s -> Printf.sprintf "\"%s\"" s
  | Int s -> Printf.sprintf "%d" s
  | Fun(ss, e) ->
    Printf.sprintf "function(%s){return %s;}"
      (String.concat "," ss)
      (show_e e)
  | Rec ses ->
    let ss = List.map (fun (s,e) -> s ^ ":" ^ show_e e) ses in
    Printf.sprintf "{%s}"
      (String.concat "," ss)
  | App(e1,e2) -> Printf.sprintf "%s(%s)" (show_e e1) (String.concat "," (List.map(fun e -> show_e e) e2))
  | Bin(e1,op,e2) ->
    Printf.sprintf "(%s %s %s)" (show_e e1) op (show_e e2)
  | If(e1,e2,e3) ->
      Printf.sprintf "(%s ? %s : %s)" (show_e e1) (show_e e2) (show_e e3)
  | Let(s,e1,e2) ->
    Printf.sprintf "(function(%s){return %s;}(%s))"
      s (show_e e2) (show_e e1)
  | LetRec(s,e1,e2) ->
    Printf.sprintf "(function(){var %s=%s;return %s;}())"
      s (show_e e1) (show_e e2)
  | Get(e1,e2) ->
    Printf.sprintf "%s[%s]" (show_e e1) (show_e e2)
  | Put(e1,e2, e3) ->
    Printf.sprintf "%s[%s] = %s" (show_e e1) (show_e e2) (show_e e3)
  | Raise -> "(throw \"error\")"
  | Match(_,_) -> assert false
  | Tuple(_) -> assert false

let rec to_if e = match e with
  | Unit -> e
  | Var s -> e
  | Str s -> e
  | Int s -> e
  | Fun(ss, e) -> Fun(ss, to_if e)
  | Rec ses -> Rec(List.map (fun (s,e)-> (s, to_if e)) ses)
  | App(e1,e2) -> App(to_if e1, List.map to_if e2)
  | Bin(e1,op,e2) -> Bin(to_if e1, op, to_if e2) 
  | Let(s,e1,e2) -> Let(s, to_if e1, to_if e2)
  | LetRec(s,e1,e2) -> LetRec(s, to_if e1, to_if e2)
  | If(e1,e2,e3) -> If(to_if e1, to_if e2, to_if e3) 
  | Raise -> Raise
  | Match(e1,ees) -> cmatch e1 ees
  | Tuple(es) -> to_if (Rec(List.mapi (fun i n -> ("_"^string_of_int i, n)) es))
  | Get(e1,e2) -> Get(to_if e1, to_if e2) 
  | Put(e1,e2,e3) -> Put(to_if e1, to_if e2, to_if e3) 
and cmatch e1 ss =

  (* 引き数だけコンパイル *)
  let r = List.fold_right begin fun e str ->
    match e with
    | Var k, w, f ->
      let ret = App(Fun([k],f), [Var k]) in
      begin match w with
        | None -> ret
        | Some(e) -> If(e, ret, str)
      end
    | (m,w,f) ->
      let rec mat (envs, conds) (m, e) =
        begin match m with
          | Var p ->
            ((p,e)::envs, conds)
          | Rec ms ->
            let conds = Bin(App(Var "typeof", [e]), "==", Str "object") :: conds in
            let conds = List.fold_left (fun conds (i,v) ->
                Bin(Str i, "in", e) :: conds
            ) conds ms in
            List.fold_left (fun env (i,v) ->
                mat env (v, Bin(e,".",Var i))
            ) (envs, conds) ms
          | _ ->
            (envs, Bin(e, "==", m) :: conds)
        end
      in
      (* mについてマッチさせる *)
      let (envs,conds) = mat ([],[]) (m, Var "_") in
      let conds = match w with None -> conds | Some(e) -> e :: conds in 
      let envs = List.rev envs in
      let ret = App(Fun(List.map fst envs,f), List.map snd envs) in
      if not(conds = []) then
        If(List.fold_left (fun tl e ->
          Bin(e, "&&", tl)
        ) (List.hd conds) (List.tl conds), ret, str)
      else
        ret
  end ss Raise in
  App(Fun(["_"], r), [e1])
  
let _ = 

  let f =
    Let("e1", Rec["op",Str "+";"left",Int 1;"right",Int 2],
    LetRec("f", Fun(["fe"],
      Match(Var "fe", [
      Rec["op",Str "+"; "left",Var "l"; "right",Var "r"], None,
          Bin(App(Var "f",[Var "l"]),"+",App(Var "f", [Var "r"]));
      Rec["op",Str "*"; "left",Var "l"; "right",Var "r"], None,
          Bin(App(Var "f",[Var "l"]),"*",App(Var "f", [Var "r"]));
      Rec["op",Str "a"; "dt",Rec["op",Str "b";"dt",Var "dt"]], None,
          App(Var "f", [Var "dt"]);
      Var "_", None, Var "_"
      ])),
    App(Var "console.log", [App(Var "f", [Var "e1"])]))) in
  Printf.printf "%s\n" (show_e (to_if f))
