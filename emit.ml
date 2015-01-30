open Syntax

(* シンプルにJavaScriptに変換する *)
let rec h oc t =
  let rec hs oc = function
    | [] -> ()
    | [x] -> Printf.fprintf oc "%a" h x
    | x::xs -> Printf.fprintf oc "%a,%a" h x hs xs
  in
  let rec idtys oc = function
    | [] -> ()
    | [(id,t)] -> Printf.fprintf oc "%s" id
    | (id,t)::xs -> Printf.fprintf oc "%s,%a" id idtys xs
  in
  let rec idtyts oc (ts,t) =

    let id1 = Id.genid "_a_" in
    Printf.fprintf oc "var %s = %a;" id1 h t;

    let _ = List.fold_left begin fun n (id,_) ->
      Printf.fprintf oc "var %s = %s[%d];" id id1 n;
      n + 1
    end 0 ts in
    ()
  in
  match t with
  | Syntax.Unit -> Printf.fprintf oc "undefined"
  | Syntax.Bool(b) -> Printf.fprintf oc "%b" b
  | Syntax.Int(i) -> Printf.fprintf oc "%d" i
  | Syntax.Float(f) -> Printf.fprintf oc "%f" f
  | Syntax.Not(t) -> Printf.fprintf oc "(!%a)" h t
  | Syntax.Neg(t) -> Printf.fprintf oc "(-%a)" h t
  | Syntax.Add(t, t2) -> Printf.fprintf oc "(%a + %a)" h t h t2
  | Syntax.Sub(t, t2) -> Printf.fprintf oc "(%a - %a)" h t h t2
  | Syntax.FNeg(t) -> Printf.fprintf oc "(-%a)" h t
  | Syntax.FAdd(t, t2) -> Printf.fprintf oc "(%a + %a)" h t h t2
  | Syntax.FSub(t, t2) -> Printf.fprintf oc "(%a - %a)" h t h t2
  | Syntax.FMul(t, t2) -> Printf.fprintf oc "(%a * %a)" h t h t2
  | Syntax.FDiv(t, t2) -> Printf.fprintf oc "(%a / %a)" h t h t2
  | Syntax.Eq(t, t2) -> Printf.fprintf oc "(%a == %a)" h t h t2
  | Syntax.LE(t, t2) -> Printf.fprintf oc "(%a <= %a)" h t h t2
  | Syntax.If(t, t2, t3) -> Printf.fprintf oc "(%a ? %a : %a)" h t h t2 h t3
  | Syntax.Let((id, Type.Unit), t, t2) -> Printf.fprintf oc "(%a,%a)" h t h t2
  | Syntax.Let((id, ty), t, t2) -> Printf.fprintf oc "(function(){\nvar %s = %a;\n return %a;\n}()\n)" id h t h t2
  | Syntax.Var(id) -> Printf.fprintf oc "%s" id
  | Syntax.LetRec({Syntax.name=(id,ty);Syntax.args=args; Syntax.body = t }, t2) ->
      Printf.fprintf oc "(function(){function %s (%a) { return %a; }\nreturn %a;}())\n" id idtys args h t h t2
  | Syntax.App(t, ts) -> Printf.fprintf oc "%a(%a)" h t hs ts
  | Syntax.Tuple(ts) -> Printf.fprintf oc "[%a]" hs ts
  | Syntax.LetTuple(its, t, t2) -> Printf.fprintf oc "(function(){%a\nreturn %a;}())" idtyts (its,t) h t2
  | Syntax.Array(t, t2) -> Printf.fprintf oc "makeArray(%a,%a)" h t h t2
  | Syntax.Get(t, t2) -> Printf.fprintf oc "%a[%a]" h t h t2
  | Syntax.Put(t, t2, t3) -> Printf.fprintf oc "%a[%a]=%a" h t h t2 h t3

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
  h oc ast

