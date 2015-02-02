open Syntax

(* シンプルにJavaScriptに変換する *)
let rec h oc t =
  let rec hs oc = function
    | [] -> ()
    | [x] -> Printf.fprintf oc "%a" h x
    | x::xs -> Printf.fprintf oc "%a,%a" h x hs xs
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
  | Syntax.LetRec(((id,ty),args, t), t2) ->
      let rec idtys oc = function
        | [] -> ()
        | [(id,t)] -> Printf.fprintf oc "%s" id
        | (id,t)::xs -> Printf.fprintf oc "%s,%a" id idtys xs
      in
      Printf.fprintf oc "(function(){function %s (%a) { return %a; }\nreturn %a;}())\n" id idtys args h t h t2
  | Syntax.App(t, ts) -> Printf.fprintf oc "%a(%a)" h t hs ts
  | Syntax.CApp(i, Syntax.Unit) -> Printf.fprintf oc "%s" i
  | Syntax.CApp(i, t) -> Printf.fprintf oc "new %s(%a)" i h t
  | Syntax.Tuple(ts) -> Printf.fprintf oc "[%a]" hs ts
  | Syntax.LetTuple(its, t, t2) ->
    let rec idtyts oc (ts,t) =
      let id1 = Id.genid "_a_" in
      Printf.fprintf oc "var %s = %a;" id1 h t;
      let _ = List.fold_left begin fun n (id,_) ->
        Printf.fprintf oc "var %s = %s[%d];" id id1 n;
        n + 1
      end 0 ts in
      ()
    in
    Printf.fprintf oc "(function(){%a\nreturn %a;}())" idtyts (its,t) h t2
  | Syntax.Array(t, t2) -> Printf.fprintf oc "makeArray(%a,%a)" h t h t2
  | Syntax.Get(t, t2) -> Printf.fprintf oc "%a[%a]" h t h t2
  | Syntax.Put(t, t2, t3) -> Printf.fprintf oc "%a[%a]=%a" h t h t2 h t3
  | Syntax.Match(t, tts) ->

    let c oc = function
      | (Syntax.Var(v),w,t) ->
        let cw oc = function
          | v,None -> ()
          | v,Some(t) -> Printf.fprintf oc "/*a*/if (function(%s){ return %a;}(__mincaml__tmp__)) " v h t
        in
        Printf.fprintf oc "\n%areturn (function(%s){return %a; }(__mincaml__tmp__));" cw (v,w) v h t
      | (Syntax.CApp(v,Syntax.Unit),w,t) ->
        let cw oc = function
          | None -> ()
          | Some(t) -> Printf.fprintf oc "/*b*/if (%a) " h t
        in
        Printf.fprintf oc "\nif(__mincaml__tmp__.tag=='%s') %a{ return %a; }" v cw w h t
      | (Syntax.CApp(v,Syntax.Var(a)),w,t) ->
        let cw oc = function
          | v,None -> ()
          | v,Some(t) -> Printf.fprintf oc "/*c*/if (function(%s){ return %a;}(__mincaml__tmp__.data)) " v h t
        in
        Printf.fprintf oc
          "\nif(__mincaml__tmp__.tag=='%s') %a{ return (function(%s){ return %a; }(__mincaml__tmp__.data));}"
          v cw (a,w) a h t
      | (Syntax.CApp(v,Syntax.Tuple(vs)),w,t) ->
        let cw oc = function
          | v,None -> ()
          | v,Some(t) -> Printf.fprintf oc "/*d*/if (function(%a){ return %a;}.apply(this,__mincaml__tmp__.data)) " hs v h t
        in
        Printf.fprintf oc
          "\nif(__mincaml__tmp__.tag=='%s') %a{ return (function(%a){ return %a; }.apply(this,__mincaml__tmp__.data));}"
            v cw (vs,w) hs vs h t
      | (t1,w,t) ->
        let cw oc = function
          | None -> ()
          | Some(t) -> Printf.fprintf oc "/*e*/if (%a) " h t
        in
        Printf.fprintf oc "\nif(__mincaml__tmp__==%a) %a{ return %a; }" h t1 cw w h t
    in
    let rec cases oc = function
      | [] -> ()
      | t :: ts -> Printf.fprintf oc "%a %a" c t cases ts
    in
    Printf.fprintf oc "(function(__mincaml__tmp__){%a}(%a))" cases tts h t
  | Syntax.Type(id,tys,t) ->
    let rec ptys oc = function
    | [] -> ()
    | (id,[])::xs ->
      Printf.fprintf oc "var %s = {tag:'%s'};\n%a" id id ptys xs
    | (id,ts)::xs ->
      Printf.fprintf oc "function %s(a){this.tag='%s';this.data=a;}\n%a" id id ptys xs
    in
    Printf.fprintf oc "%a%a" ptys tys h t

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

