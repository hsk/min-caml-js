open Syntax
let rec show_e = function
  | Unit -> "undefined"
  | Var s -> s
  | Str s -> Printf.sprintf "\"%s\"" s
  | Int s -> Printf.sprintf "%d" s
  | Float f -> Printf.sprintf "%f" f
  | Bool b -> Printf.sprintf "%b" b
  | Fun(ss, e) ->
    Printf.sprintf "function(%s){return %s;}"
      (String.concat "," ss)
      (show_e e)
  | Rec ses ->
    let ss = List.map (fun (s,e) -> s ^ ":" ^ show_e e) ses in
    Printf.sprintf "{%s}"
      (String.concat "," ss)
  | App(e1,e2) -> Printf.sprintf "%s(%s)" (show_e e1) (String.concat "," (List.map(fun e -> show_e e) e2))
  | Pre(op,e1) ->
    Printf.sprintf "(%s %s)" op (show_e e1)
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
  | Raise -> "(function(){throw \"error\";}())"
  | Match(_,_) -> assert false
  | Tuple(es) -> assert false
  | Array(e1, e2) -> Printf.sprintf "makeArray(%s,%s)" (show_e e1) (show_e e2)
  | CApp(i, t) -> assert false
  | Type(id,tys,t) -> (show_e t)

let rec to_if e = match e with
  | Unit -> e
  | Var s -> e
  | Str s -> e
  | Int s -> e
  | Float _ -> e
  | Bool _ -> e
  | Pre (op, e1) -> Pre(op, to_if e1) 
  | Array (e1, e2) -> Array(to_if e1, to_if e2) 
  | Fun(ss, e) -> Fun(ss, to_if e)
  | Rec ses -> Rec(List.map (fun (s,e)-> (s, to_if e)) ses)
  | App(e1,e2) -> App(to_if e1, List.map to_if e2)
  | Bin(e1,op,e2) -> Bin(to_if e1, op, to_if e2) 
  | Let(s,e1,e2) -> Let(s, to_if e1, to_if e2)
  | LetRec(s,e1,e2) -> LetRec(s, to_if e1, to_if e2)
  | If(e1,e2,e3) -> If(to_if e1, to_if e2, to_if e3) 
  | Raise -> Raise
  | Match(e1,ees) -> cmatch (to_if e1) ees
  | Tuple(es) -> to_if (Rec(List.mapi (fun i n -> ("_"^string_of_int i, n)) es))
  | Get(e1,e2) -> Get(to_if e1, to_if e2) 
  | Put(e1,e2,e3) -> Put(to_if e1, to_if e2, to_if e3) 
  | CApp(e1,e2) -> Rec([("tag", Str e1); ("data",to_if e2)] )
  | Type(id,tys,t) -> Type(id,tys,to_if t)

and cmatch e1 ss =

  (* 引き数だけコンパイル *)
  let r = List.fold_right begin fun e str ->
    let (m,w,f) = e in

    match (to_if m,w,to_if f) with
    | Var k, w, f ->
      let ret = App(Fun([k],f), [Var "_"]) in
      begin match w with
        | None -> ret
        | Some(e) -> If(App(Fun([k],to_if e), [Var "_"]) , ret, str)
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
      let envs = List.rev envs in
      let conds = match w with
        | None -> conds
        | Some(e) ->
          App(Fun(List.map fst envs, to_if e), List.map snd envs) :: conds
      in 
      let ret = App(Fun(List.map fst envs,f), List.map snd envs) in
      if not(conds = []) then
        If(List.fold_left (fun tl e ->
          Bin(e, "&&", tl)
        ) (List.hd conds) (List.tl conds), ret, str)
      else
        ret
  end ss Raise in
  App(Fun(["_"], r), [e1])

let f oc ast =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "// generating by mincaml2js\n";
  Printf.fprintf oc "function print_int(n) { console._stdout.write(\"\"+n);}\n";
  Printf.fprintf oc "function makeArray(n,v) { var a = []; for(var i = 0; i < n; i++) a[i] = v; return a; }\n";
  Printf.fprintf oc "var abs_float = Math.abs;\n";
  Printf.fprintf oc "var sqrt = Math.sqrt;\n";
  Printf.fprintf oc "var sin = Math.sin;\n";
  Printf.fprintf oc "var cos = Math.cos;\n";
  Printf.fprintf oc "var int_of_float = Math.floor;\n";
  Printf.fprintf oc "function truncate(a) { return a >= 0 ? Math.floor(a) : -Math.floor(-a); }\n";
  Printf.fprintf oc "function float_of_int(a){return a+0.0;}\n";
  Printf.fprintf oc "function print_newline(){console.log(\"\");}\n";
  Printf.fprintf oc "%s\n" (show_e (to_if ast))

