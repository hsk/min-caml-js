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

(*
ネストしたletを平らにする


let a =
  let a = 1 in
  a

は

let a = 1 in
let a = a

ただし、このネストしているletはスコープを作るのでうざい。

let x = 10 in
let a =
  let x = 1 in
  x
in a + x

を

let x = 10 in
let x = 1 in
let a = x
in a + x

とするとおかしくなるので

let x = 10 in
let x_a = 1 in
let a = x_a
in a + x

と名前を変更する必要がある。

let rec add a b = a + b in
let rec sub a b = a + b in


*)
module Alpha = struct

  let find x env = try M.find x env with Not_found -> x

  let rec g env = function

    | Syntax.Let((x,t), e1, e2) ->
      let x' = Id.genid x in
      Syntax.Let((x', t), g env e1, g (M.add x x' env) e2)

    | Syntax.LetRec({Syntax.name=(x,t);Syntax.args=yts; Syntax.body = e1 }, e2) ->
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      Syntax.LetRec({
          Syntax.name =(find x env,t);
          Syntax.args = List.map (fun (y, t) -> (find y env', t)) yts;
          Syntax.body = g env' e1 },
        g env e2)
    | Syntax.LetTuple(xts, y, e) ->

      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      Syntax.LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
         g env y,
         g env' e)

    | Syntax.Var(id) -> Syntax.Var(find id env)
    | Syntax.Tuple(ts) -> Syntax.Tuple(List.map (g env) ts)

    | Syntax.Not(t) -> Syntax.Not(g env t)
    | Syntax.Neg(t) -> Syntax.Neg(g env t) 
    | Syntax.Add(t, t2) -> Syntax.Add(g env t, g env t2)
    | Syntax.Sub(t, t2) -> Syntax.Sub(g env t, g env t2)
    | Syntax.FNeg(t) -> Syntax.FNeg(g env t)
    | Syntax.FAdd(t, t2) -> Syntax.FAdd(g env t, g env t2)
    | Syntax.FSub(t, t2) -> Syntax.FSub(g env t, g env t2)
    | Syntax.FMul(t, t2) -> Syntax.FMul(g env t, g env t2)
    | Syntax.FDiv(t, t2) -> Syntax.FDiv(g env t, g env t2)
    | Syntax.Eq(t, t2) -> Syntax.Eq(g env t, g env t2)
    | Syntax.LE(t, t2) -> Syntax.LE(g env t, g env t2)
    | Syntax.If(t, t2, t3) -> Syntax.If(g env t, g env t2, g env t3)
    | Syntax.App(t, ts) -> Syntax.App(g env t, List.map (g env) ts)
    | Syntax.Array(t, t2) -> Syntax.Array(g env t, g env t2)
    | Syntax.Get(t, t2) -> Syntax.Get(g env t, g env t2)
    | Syntax.Put(t, t2, t3) -> Syntax.Put(g env t, g env t2, g env t3)
    | e -> e

  let f = g M.empty
end

module Assoc = struct

  let find x env = try M.find x env with Not_found -> x

  let rec get_let2 env t f1 =
    match t with
    | Syntax.Let(id1, t1, t2) ->
      Syntax.Let(id1, g env t1, f1 env t2)

    | Syntax.LetRec({Syntax.name=id1;Syntax.args=args1; Syntax.body = t1 }, t2) ->
      Syntax.LetRec({Syntax.name=id1;Syntax.args=args1; Syntax.body = g env t1 }, f1 env t2)

    | Syntax.LetTuple(nm, t1, t2) ->
      Syntax.LetTuple(nm, g env t1, f1 env t2)
    | t -> f1 env (g env t)

  and g env = function

    | Syntax.Let(id, t, t3) ->
      get_let2 env t (fun env t -> Syntax.Let(id, g env t, g env t3))

    | Syntax.LetRec({Syntax.name=id;Syntax.args=args; Syntax.body = t }, t3) ->
      Syntax.LetRec({Syntax.name=id;Syntax.args=args; Syntax.body = g env t }, g env t3)
    | Syntax.LetTuple(its, t, t2) ->
      get_let2 env t (fun env t -> Syntax.LetTuple(its, g env t, g env t2))

    | Syntax.Var(id) -> Syntax.Var(id)
    | Syntax.Tuple(ts) -> Syntax.Tuple(List.map (g env) ts)

    | Syntax.Not(t) -> Syntax.Not(g env t)
    | Syntax.Neg(t) -> Syntax.Neg(g env t) 
    | Syntax.Add(t, t2) -> Syntax.Add(g env t, g env t2)
    | Syntax.Sub(t, t2) -> Syntax.Sub(g env t, g env t2)
    | Syntax.FNeg(t) -> Syntax.FNeg(g env t)
    | Syntax.FAdd(t, t2) -> Syntax.FAdd(g env t, g env t2)
    | Syntax.FSub(t, t2) -> Syntax.FSub(g env t, g env t2)
    | Syntax.FMul(t, t2) -> Syntax.FMul(g env t, g env t2)
    | Syntax.FDiv(t, t2) -> Syntax.FDiv(g env t, g env t2)
    | Syntax.Eq(t, t2) -> Syntax.Eq(g env t, g env t2)
    | Syntax.LE(t, t2) -> Syntax.LE(g env t, g env t2)
    | Syntax.If(t, t2, t3) -> Syntax.If(g env t, g env t2, g env t3)
    | Syntax.App(t, ts) -> Syntax.App(g env t, List.map (g env) ts)
    | Syntax.Array(t, t2) -> Syntax.Array(g env t, g env t2)
    | Syntax.Get(t, t2) -> Syntax.Get(g env t, g env t2)
    | Syntax.Put(t, t2, t3) -> Syntax.Put(g env t, g env t2, g env t3)
    | e -> e

(*
  右辺にIf、Letを許さない変換を行う。


  
*)
  let rec h oc = function
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
    | Syntax.Var(id) -> Printf.fprintf oc "%s" id
    | Syntax.App(t, ts) -> Printf.fprintf oc "%a(%a)" h t hs ts
    | Syntax.Tuple(ts) -> Printf.fprintf oc "[%a]" hs ts
    | Syntax.Array(t, t2) -> Printf.fprintf oc "makeArray(%a,%a)" h t h t2
    | Syntax.Get(t, t2) -> Printf.fprintf oc "%a[%a]" h t h t2
    | Syntax.Put(t, t2, t3) -> Printf.fprintf oc "%a[%a]=%a" h t h t2 h t3

    | Syntax.If(t, t2, t3) -> Printf.fprintf oc "(%a ? %a : %a)" h t h t2 h t3
    | Syntax.Let((id, Type.Unit), t, t2) -> Printf.fprintf oc "(%a,%a)" h t h t2
    | Syntax.Let((id, ty), t, t2) -> Printf.fprintf oc "(function(){\nvar %s = %a;\n return %a;\n}()\n)" id h t h t2
    | Syntax.LetRec({Syntax.name=(id,ty);Syntax.args=args; Syntax.body = t }, t2) ->
        Printf.fprintf oc "(function(){function %s (%a) { return %a; }\nreturn %a;}())\n" id idtys args h t h t2
    | Syntax.LetTuple(its, t, t2) -> Printf.fprintf oc "(function(){%a\nreturn %a;}())" idtyts (its,t) h t2

  and hs oc = function
    | [] -> ()
    | [x] -> Printf.fprintf oc "%a" h x
    | x::xs -> Printf.fprintf oc "%a,%a" h x hs xs
  and idtys oc = function
    | [] -> ()
    | [(id,t)] -> Printf.fprintf oc "%s" id
    | (id,t)::xs -> Printf.fprintf oc "%s,%a" id idtys xs
  and idtyts oc (ts,t) =
    let id1 = Id.genid "_a_" in
    Printf.fprintf oc "var %s = %a;" id1 h t;

    let _ = List.fold_left begin fun n (id,_) ->
      Printf.fprintf oc "var %s = %s[%d];" id id1 n;
      n + 1
    end 0 ts in
    ()

  let f oc e = h oc (g M.empty (Alpha.f e))
end

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
  Assoc.f oc ast
  (* h oc ast *)

