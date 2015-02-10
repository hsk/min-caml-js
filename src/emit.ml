open Syntax

let rec to_if e = match e with
  | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
  | Pre (op, e1) -> Pre(op, to_if e1) 
  | Array (e1, e2) -> Array(to_if e1, to_if e2) 
  | Fun(ss, e) -> Fun(ss, to_if e)
  | Rec ses -> let rec f e = match e with (s,e) -> (s, to_if e) in Rec(List.map f ses)
  | App(e1,e2) -> App(to_if e1, List.map to_if e2)
  | Bin(e1,op,e2) -> Bin(to_if e1, op, to_if e2) 
  | Let(s,e1,e2) -> Let(s, to_if e1, to_if e2)
  | LetRec(s,e1,e2) -> LetRec(s, to_if e1, to_if e2)
  | If(e1,e2,e3) -> If(to_if e1, to_if e2, to_if e3) 
  | Tuple(es) -> let rec f i n = ("_"^string_of_int i, n) in to_if (Rec(List.mapi f es))
  | Get(e1,e2) -> Get(to_if e1, to_if e2) 
  | Put(e1,e2,e3) -> Put(to_if e1, to_if e2, to_if e3) 
  | CApp(e1,e2) -> Rec([("tag", Str e1); ("data",to_if e2)] )
  | Match(e1,ss) ->
    let rec mat e me =
      match (e,me) with
      | (envs, conds), (Bin(m,"as",Var p), e) -> mat ((p,e)::envs, conds) (m, e)
      | (envs, conds), (Var p, e) -> ((p,e)::envs, conds)
      | (envs, conds), (Rec ms, e) ->
        let f env iv = match iv with
          | (i,v) -> mat env (v, Bin(e,".",Var i))
        in
        List.fold_left f (envs, conds) ms
      | (envs, conds), (m, e) ->
        (envs, Bin(e, "==", m) :: conds)
    in
    let f mwf str =
      let (m, w, f) = mwf in
      let (m, f) = (to_if m, to_if f) in
      let (envs,conds) = mat ([],[]) (m, Var "_") in
      let envs = List.rev envs in
      let conds = match w with
        | None -> conds
        | Some(e) ->
          App(Fun(List.map fst envs, to_if e), List.map snd envs) :: conds
      in 
      let ret = App(Fun(List.map fst envs,f), List.map snd envs) in
      if conds = [] then
        ret
      else
        let f tl e = Bin(e, "&&", tl) in
        let cond = List.fold_left f (List.hd conds) (List.tl conds) in
        If(cond, ret, str)
    in
    let r = List.fold_right f ss (Raise "error") in
    App(Fun(["_"], r), [to_if e1])

let rec show_e e = match e with
  | Unit -> "undefined"
  | Var s -> s
  | Str s -> Printf.sprintf "\"%s\"" s
  | Int s -> Printf.sprintf "%d" s
  | Float f -> Printf.sprintf "%f" f
  | Bool b -> Printf.sprintf "%b" b
  | Fun(ss, e) ->
    Printf.sprintf "function(%s){return %s;}"
      (String.concat "," ss) (show_e e)
  | Rec ses ->
    let rec f se = match se with
      | (s,e) -> s ^ ":" ^ show_e e
    in
    let ss = List.map f ses in
    Printf.sprintf "{%s}" (String.concat "," ss)
  | App(e1,e2) ->
    Printf.sprintf "%s(%s)"
      (show_e e1) (String.concat "," (List.map show_e e2))
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
  | Raise(s) -> Printf.sprintf "(function(){throw \"%s\";}())" s
  | Array(e1, e2) -> Printf.sprintf "makeArray(%s,%s)" (show_e e1) (show_e e2)
  | Match(_,_) | Tuple(_) | CApp(_, _) -> assert false

let f oc ast =
  Format.eprintf "generating javascript...@.";
  Printf.fprintf oc "function print_int(n) { console._stdout.write(\"\"+n);}\n";
  Printf.fprintf oc "var print_string = print_int;\n";
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
