open Syntax

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
  | Array(_, _) | Match(_,_) | Tuple(_) | CApp(_, _) -> assert false

let f oc ast =
  Format.eprintf "generating javascript...@.";
  Printf.fprintf oc "function print_int(n) { console._stdout.write(\"\"+n);}\n";
  Printf.fprintf oc "var print_string = print_int;\n";
  Printf.fprintf oc "function makeArray(n,v) { var a = []; for(var i = 0; i < n; i++) a[i] = v; return a; }\n";
  Printf.fprintf oc "function ref(n) { return {ref:n}; }\n";
  Printf.fprintf oc "var abs_float = Math.abs;\n";
  Printf.fprintf oc "var sqrt = Math.sqrt;\n";
  Printf.fprintf oc "var sin = Math.sin;\n";
  Printf.fprintf oc "var cos = Math.cos;\n";
  Printf.fprintf oc "var int_of_float = Math.floor;\n";
  Printf.fprintf oc "function truncate(a) { return a >= 0 ? Math.floor(a) : -Math.floor(-a); }\n";
  Printf.fprintf oc "function float_of_int(a){return a+0.0;}\n";
  Printf.fprintf oc "function print_newline(){console.log(\"\");}\n";
  Printf.fprintf oc "%s\n" (show_e (To_if.f ast))
