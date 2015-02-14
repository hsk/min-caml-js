open Syntax

let rec show_e e = match e with
  | Unit -> "undefined"
  | Var "String.length" -> "String.length_"
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
  | Array(_, _) | Match(_,_) | Tuple(_) | CApp(_, _) -> assert false

let f oc ast =
  Format.eprintf "generating javascript...@.";
  Printf.fprintf oc "%s\n" (Syntax.read_all "../libs/lib.js");
  Printf.fprintf oc "%s\n" (show_e (To_if.f ast))
