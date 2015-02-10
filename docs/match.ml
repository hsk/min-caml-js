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
  | Match(e1,ees) -> Printf.sprintf "%s(%s)" (cmatch ees) (show_e e1)
  | Let(s,e1,e2) ->
    Printf.sprintf "(function(%s){return %s;}(%s))"
      s (show_e e2) (show_e e1)
  | LetRec(s,e1,e2) ->
    Printf.sprintf "(function(){var %s=%s;return %s;}())"
      s (show_e e1) (show_e e2)
and cmatch ss =
  (* 例外文字列 *)
  let throws = ref "   throw ('cannot match '+e);\n" in

  (* 引き数だけコンパイル *)
  let str = List.fold_left begin fun str e ->
    match e with
    | Var "_", w, f ->
      throws := "";
      let ret = "    return ("^show_e f^"(e));\n" in
      begin match w with
        | None ->
          str ^ ret
        | Some(e) ->
          str ^ "    if (" ^ show_e e ^ ") {\n" ^ ret ^ "    }\n"
      end
    | (m,w,f) ->
      (* envsとcondsにmat関数の結果を入れる *)
      let rec mat (envs, conds) (m, e) =
        begin match m with
          | Var p ->
            (* パターン変数なら名前と、eを返す。 *)
            ((p ^ ":" ^ e)::envs, conds)
          | Obj ms ->
            (* ただのオブジェクトならオブジェクトチェックと *)
            let conds = ("typeof "^e^" == 'object'") :: conds in
            (* １こ１この条件を条件に加え *)

            let conds = List.fold_left (fun conds (i,v) ->
                ("\""^i^"\" in "^e) :: conds
            ) conds ms in
            (* 更に、再帰的に検索する *)
            List.fold_left (fun env (i,v) ->
                mat env (v, e^"."^i)
            ) (envs, conds) ms
          (* 通常データなら、比較条件とする。 *)
          | _ ->
            (envs, (e ^ " == " ^ show_e m) :: conds)
        end
      in
      (* mについてマッチさせる *)
      let (envs,conds) = mat ([],[]) (m, "_") in
      let conds = match w with None -> conds | Some(e) -> (show_e e) :: conds in 
      let (envs,conds) = (List.rev envs, List.rev conds) in
      let ret =  "        return (" ^ (show_e f) ^ "({" ^ (String.concat "," envs) ^ "}));\n" in
      (* 条件があれば条件を出力 *)
      if not(conds = []) then
        str ^ "    if (" ^ (String.concat " && " conds) ^ ") {\n" ^ ret ^ "    }\n"
      else
        str ^ ret
  end "" ss in
  (* 最後に例外と *)
  "function(_) {\n" ^ str ^ !throws ^ "}\n"

let _ = 

  let f =
    Let("e", Obj["op",Str "+";"left",Int 1;"right",Int 2],
    LetRec("f", Fun(["e"],
      Match(Var "e", [
      Obj["op",Str "+"; "left",Var "l"; "right",Var "r"], None,
          Fun(["e"], Add(App(Var "f",Var "e.l"),App(Var "f", Var "e.r")));
      Obj["op",Str "*"; "left",Var "l"; "right",Var "r"], None,
          Fun(["e"], Mul(App(Var "f",Var "e.l"),App(Var "f", Var "e.r")));
      Obj["op",Str "a"; "dt",Obj["op",Str "b";"dt",Var "dt"]], None,
          Fun(["e"], App(Var "f", Var "e.dt"));
      Var "_", None, Fun(["e"], Var "e")
      ])),
    App(Var "console.log", App(Var "f", Var "e")))) in
  Printf.printf "%s\n" (show_e f)
