type e =
  | Obj of (string * e) list
  | Str of string
  | Var of string
  | Fun of string list * e
  | Add of e * e
  | Mul of e * e
  | App of e * e
  | Match of e * (e * e option * e) list
  | Let of string * e * e
  | LetRec of string * e * e
  | Int of int
  | If of e * e * e
  | Raise
  | Bin of e * string * e
(*[@@deriving show]*)

let rec to_if e = match e with
  | Var s -> e
  | Str s -> e
  | Int s -> e
  | Fun(ss, e) -> Fun(ss, to_if e)
  | Obj ses -> Obj(List.map (fun (s,e)-> (s, to_if e)) ses)
  | App(e1,e2) -> App(to_if e1, to_if e2)
  | Add(e1,e2) -> Add(to_if e1, to_if e2) 
  | Bin(e1,op,e2) -> Bin(to_if e1, op, to_if e2) 
  | Mul(e1,e2) -> Mul(to_if e1, to_if e2)
  | Let(s,e1,e2) -> Let(s, to_if e1, to_if e2)
  | LetRec(s,e1,e2) -> LetRec(s, to_if e1, to_if e2)
  | If(e1,e2,e3) -> If(to_if e1, to_if e2, to_if e3) 
  | Match(e1,ees) -> cmatch e1 ees
  | Raise -> Raise
and cmatch e1 ss =

  (* 引き数だけコンパイル *)
  let r = List.fold_right begin fun e str ->
    match e with
    | Var "_", w, f ->
      let ret = App(f, Var "_") in
      begin match w with
        | None -> ret
        | Some(e) -> If(e, ret, str)
      end
    | (m,w,f) ->
      let rec mat (envs, conds) (m, e) =
        begin match m with
          | Var p ->
            ((p,e)::envs, conds)
          | Obj ms ->
            let conds = Bin(App(Var "typeof", e), "==", Str "object") :: conds in
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
      let ret = App(f, Obj envs) in
      if not(conds = []) then
        If(List.fold_left (fun tl e ->
          Bin(e, "&&", tl)
        ) (List.hd conds) (List.tl conds), ret, str)
      else
        ret
  end ss Raise in
  App(Fun(["_"], r), e1)
let rec show_e = function
  | Var s -> s
  | Str s -> Printf.sprintf "\"%s\"" s
  | Int s -> Printf.sprintf "%d" s
  | Fun(ss, e) ->
    Printf.sprintf "function(%s){return %s;}"
      (String.concat "," ss)
      (show_e e)
  | Obj ses ->
    let ss = List.map (fun (s,e) -> s ^ ":" ^ show_e e) ses in
    Printf.sprintf "{%s}"
      (String.concat "," ss)
  | App(e1,e2) -> Printf.sprintf "%s(%s)" (show_e e1) (show_e e2)
  | Add(e1,e2) -> Printf.sprintf "(%s + %s)" (show_e e1) (show_e e2)
  | Mul(e1,e2) -> Printf.sprintf "(%s * %s)" (show_e e1) (show_e e2)
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
  | Raise -> "(throw \"error\")"
  | Match(_,_) -> assert false
  
let _ = 

  let f =
    Let("e", Obj["op",Str "+";"left",Int 1;"right",Int 2],
    LetRec("f", Fun(["fe"],
      Match(Var "fe", [
      Obj["op",Str "+"; "left",Var "l"; "right",Var "r"], None,
          Fun(["e"], Add(App(Var "f",Var "e.l"),App(Var "f", Var "e.r")));
      Obj["op",Str "*"; "left",Var "l"; "right",Var "r"], None,
          Fun(["e"], Mul(App(Var "f",Var "e.l"),App(Var "f", Var "e.r")));
      Obj["op",Str "a"; "dt",Obj["op",Str "b";"dt",Var "dt"]], None,
          Fun(["e"], App(Var "f", Var "e.dt"));
      Var "_", None, Fun(["e"], Var "e")
      ])),
    App(Var "console.log", App(Var "f", Var "e")))) in
  Printf.printf "%s\n" (show_e (to_if f))
