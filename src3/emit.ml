
module To_if = struct
  open Syntax

  let rec f e = match e with
    | Unit | Var _ | Str _ | Int _ | Float _ | Bool _ | Raise _ -> e
    | Pre (op, e1) -> Pre(op, f e1) 
    | Array (e1, e2) -> App(Var "makeArray", [f e1; f e2]) 
    | Fun(ss, e) -> Fun(ss, f e)
    | Rec ses -> let rec f1 e = match e with (s,e) -> (s, f e) in Rec(List.map f1 ses)
    | App(e1,e2) -> App(f e1, List.map f e2)
    | Bin(e1,op,e2) -> Bin(f e1, op, f e2) 
    | Let(s,e1,e2) -> Let(s, f e1, f e2)
    | LetRec(s,e1,e2) -> LetRec(s, f e1, f e2)
    | If(e1,e2,e3) -> If(f e1, f e2, f e3) 
    | Tuple(es) -> let rec f1 i n = ("_"^string_of_int i, n) in f (Rec(List.mapi f1 es))
    | Get(e1,e2) -> Get(f e1, f e2) 
    | Put(e1,e2,e3) -> Put(f e1, f e2, f e3) 
    | CApp(e1,e2) -> Rec([("tag", Str e1); ("data",f e2)] )
    | Match(e1,ss) ->
      let rec mat e me =
        match (e,me) with
        | (envs, conds), (Bin(m,"as",Var p), e) -> mat ((p,e)::envs, conds) (m, e)
        | (envs, conds), (Var p, e) -> ((p,e)::envs, conds)
        | (envs, conds), (Rec ms, e) ->
          let f1 env iv = match iv with
            | (i,v) -> mat env (v, Bin(e,".",Var i))
          in
          List.fold_left f1 (envs, conds) ms
        | (envs, conds), (m, e) ->
          (envs, Bin(e, "==", m) :: conds)
      in
      let f1 mwf str =
        let (m, w, f1) = mwf in
        let (m, f1) = (f m, f f1) in
        let (envs,conds) = mat ([],[]) (m, Var "_") in
        let envs = List.rev envs in
        let conds = match w with
          | None -> conds
          | Some(e) ->
            App(Fun(List.map fst envs, f e), List.map snd envs) :: conds
        in 
        let ret = App(Fun(List.map fst envs,f1), List.map snd envs) in
        if conds = [] then
          ret
        else
          let f1 tl e = Bin(e, "&&", tl) in
          let cond = List.fold_left f1 (List.hd conds) (List.tl conds) in
          If(cond, ret, str)
      in
      let r = List.fold_right f1 ss (Raise "error") in
      App(Fun(["_"], r), [f e1])
end

module Alpha = struct
  open Syntax
  let find env i = if List.mem_assoc i env then List.assoc i env else i
  let mem env i = List.mem_assoc i env
  let rec g env = function
    | Int(i) -> Int(i)
    | Float(i) -> Float(i)
    | Str(i) -> Str(i)
    | Raise(i) -> Raise(i)
    | Bool(i) -> Bool(i)
    | Var(s) -> Var(find env s)
    | Unit -> Unit
    | Bin(e1,op,e2) -> Bin(g env e1,op, g env e2)
    | Pre(op,e1) -> Pre(op, g env e1)
    | App(e,es) -> App(g env e, List.map (g env) es)
    | If(e1,e2,e3) -> If(g env e1, g env e2, g env e3)
    | Let(s,e1,e2) ->
      let s' = if mem env s then genid s else s in
      Let(s',g env e1, g ((s,s')::env) e2)
    | LetRec(s, e1, e2) ->
      let s' = if mem env s then genid s else s in
      LetRec(s', g ((s,s')::env) e1, g ((s,s')::env) e2)
    | Fun(is,e) ->
      let iis = List.map(fun i -> (i,i)) is in
      Fun(is, g (iis @ env) e)  
    | Rec(ies) ->
      Rec(List.map(fun(i,e)->(i,g env e)) ies)
    | Get(e1,e2) -> Get(g env e1, g env e2)
    | Put(e1,e2,e3) -> Put(g env e1, g env e2, g env e3)
    | e -> Format.printf "error %a@." show_e e; assert false

  let f e = g [] e
end

module Javascript = struct
  type j =
    | JInt of int
    | JFloat of float
    | JVar of string
    | JBin of j * string * j
    | JGet of j * j
    | JPut of j * j * j
    | JPre of string * j
    | JApp of j * j list
    | JIf of j * j * j
    | JFun of string list * s
    | JUndefined
    | JRec of (string * j) list
    | JStr of string
    | JRaise of string
    | JBool of bool

  and s =
    | SVar of string * j
    | SDef of string
    | SAssign of string * j
    | SExp of j
    | SIf of j * s * s
    | SFun of string * string list * s
    | SRet of j
    | SCons of s * s
    | SNil

  let rec ps p fp = function
    | [] -> ()
    | [x] -> Format.fprintf fp "%a" p x
    | x::xs -> Format.fprintf fp "%a;%a" p x (ps p) xs

  let p_i fp i =
    Format.fprintf fp "\"%s\"" i

  let rec p_is fp is = 
    Format.fprintf fp "[%a]@?" (ps p_i) is

  let rec p_j fp = function
    | JInt(i) -> Format.fprintf fp "JInt(%d)" i
    | JFloat(i) -> Format.fprintf fp "JFloat(%f)" i
    | JBool(i) -> Format.fprintf fp "JBool(%b)" i
    | JRaise(s) -> Format.fprintf fp "JRaise(\"%s\")" s
    | JVar(s) -> Format.fprintf fp "JVar(\"%s\")" s
    | JStr(s) -> Format.fprintf fp "JStr(\"%s\")" s
    | JUndefined -> Format.fprintf fp "JUndefined"
    | JBin(e1,op,e2) -> Format.fprintf fp "JBin(%a, %s, %a)" p_j e1 op p_j e2
    | JGet(e1,e2) -> Format.fprintf fp "JGet(%a, %a)" p_j e1 p_j e2
    | JPut(e1,e2,e3) -> Format.fprintf fp "JPut(%a, %a, %a)" p_j e1 p_j e2 p_j e3
    | JPre(op,e1) -> Format.fprintf fp "JPre(%s, %a)" op p_j e1
    | JApp(e,es) -> Format.fprintf fp "JApp(%a, %a)" p_j e p_js es
    | JIf(e1,e2,e3) -> Format.fprintf fp "JIf(%a, %a, %a)" p_j e1 p_j e2 p_j e3
    | JFun(is, s) -> Format.fprintf fp "JFun(%a, %a)" p_is is p_s s
    | JRec(ies) -> Format.fprintf fp "JRec(%a)" p_ijs ies
  and p_ij fp (i,j) =
    Format.fprintf fp "(\"%s\",%a)" i p_j j

  and p_js fp js =
    Format.fprintf fp "[%a]@?" (ps p_j) js
  and p_ijs fp js =
    Format.fprintf fp "[%a]@?" (ps p_ij) js
  and p_s fp = function
    | SVar(i,j) -> Format.fprintf fp "SVar(\"%s\",%a)" i p_j j
    | SDef(i) -> Format.fprintf fp "SDef(\"%s\")" i
    | SAssign(i,j) -> Format.fprintf fp "SAssign(\"%s\",%a)" i p_j j
    | SExp(j) -> Format.fprintf fp "SExp(%a)" p_j j
    | SRet(j) -> Format.fprintf fp "SRet(%a)" p_j j
    | SIf(j,s1, s2) -> Format.fprintf fp "SIf(%a, %a, %a)" p_j j p_s s1 p_s s2
    | SFun(i,is, s) -> Format.fprintf fp "SFun(%a, %a, %a)" p_i i p_is is p_s s
    | SCons(s1,s2) -> Format.fprintf fp "SCons(%a, %a)" p_s s1 p_s s2
    | SNil -> Format.fprintf fp "SNil"
  and p_ss fp ss =
    Format.fprintf fp "[%a]@?" (ps p_s) ss

  open Syntax

  let (@@) a b =
    if a = SNil then b else
    if b = SNil then a else SCons(a,b)

  let rec cnve = function
    | Int(i) -> SNil,JInt(i)
    | Float(i) -> SNil,JFloat(i)
    | Var(i) -> SNil,JVar(i)
    | Str(i) -> SNil,JStr(i)
    | Raise(i) -> SNil,JRaise(i)
    | Bool(i) -> SNil,JBool(i)
    | Unit -> SNil,JUndefined
    | Bin(e1,op,e2) ->
      let s1,j1 = cnve e1 in
      let s2,j2 = cnve e2 in
      s1 @@ s2, JBin(j1, op, j2)
    | Get(e1,e2) ->
      let s1,j1 = cnve e1 in
      let s2,j2 = cnve e2 in
      s1 @@ s2, JGet(j1, j2)
    | Put(e1,e2,e3) ->
      let s1,j1 = cnve e1 in
      let s2,j2 = cnve e2 in
      let s3,j3 = cnve e3 in
      s1 @@ s2 @@ s3, JPut(j1, j2, j3)
    | Pre(op,e1) ->
      let s1,j1 = cnve e1 in
      s1, JPre(op, j1)
    | App(e,es) ->
      let s,j = cnve e in
      let ss,js = List.fold_right (fun e (ls,es) ->
        let l,e = cnve e in
        l @@ ls, (e::es)
      ) es (SNil,[]) in
      s @@ ss, JApp(j, js)
    | If(e1,e2,e3) ->
      let s1,j1 = cnve e1 in
      let s2,j2 = cnve e2 in
      let s3,j3 = cnve e3 in
      let i = genid "tmp" in
      SDef(i) @@ s1 @@ SIf(j1, s2 @@ SAssign(i,j2), s3 @@ SAssign(i,j3)), JVar(i)
    | Let(i,e1,e2) ->
      let s1, j1 = cnve e1 in
      let s2, j2 = cnve e2 in
      s1 @@ SVar(i, j1) @@ s2, j2
    | LetRec(i, Fun(is, e1), e2) ->
      let s1 = cnv true e1 in
      let s2, j2 = cnve e2 in
      SFun(i, is, s1) @@ s2, j2

    | Fun(is, e1) ->
      let s1 = cnv true e1 in
      SNil, JFun(is, s1)
    | Rec(ies) ->

      let ss,js = List.fold_right (fun (i,e) (ls,es) ->
        let l,e = cnve e in
        l @@ ls, ((i,e)::es)
      ) ies (SNil,[]) in
      ss,JRec(js)

    | e -> Format.fprintf Format.std_formatter "error %a@." show_e e; assert false

  and cnv tl = function
    | Let(i,e1,e2) ->
      let s1, j1 = cnve e1 in
      s1 @@ SVar(i, j1) @@ cnv tl e2
    | LetRec(i, Fun(is, e1), e2) ->
      let s1 = cnv true e1 in
      SFun(i, is, s1) @@ cnv tl e2
    | If(e1, e2, e3) when tl ->
      let s1, j1 = cnve e1 in
      let s2 = cnv true e2 in
      let s3 = cnv true e3 in
      s1 @@ SIf(j1, s2, s3)
    | e when tl ->
      let s, j = cnve e in
      s @@ SRet j
    | e ->
      let s, j = cnve e in
      s @@ SExp j

  let f e = cnv false e
end

open Javascript

let rec ns p fp = function
  | [] -> ()
  | [x] -> Format.fprintf fp "%a" p x
  | x::xs -> Format.fprintf fp "%a %a" p x (ns p) xs

let rec os p fp = function
  | [] -> ()
  | [x] -> Format.fprintf fp "%a" p x
  | x::xs -> Format.fprintf fp "%a,%a" p x (os p) xs
let o_i fp i =
  Format.fprintf fp "%s" i
let rec o_is fp is = 
  Format.fprintf fp "%a@?" (os o_i) is
let rec o_j fp = function
  | JInt(i) -> Format.fprintf fp "%d" i
  | JFloat(i) -> Format.fprintf fp "%f" i
  | JVar(s) -> Format.fprintf fp "%s" s
  | JStr(s) -> Format.fprintf fp "\"%s\"" s
  | JBool(i) -> Format.fprintf fp "%b" i
  | JRaise(s) -> Format.fprintf fp "(function(){throw \"%s\";}())" s
  | JUndefined -> Format.fprintf fp "undefined"
  | JBin(e1,op,e2) -> Format.fprintf fp "(%a %s %a)" o_j e1 op o_j e2
  | JGet(e1,e2) -> Format.fprintf fp "%a[%a]" o_j e1 o_j e2
  | JPut(e1,e2,e3) -> Format.fprintf fp "%a[%a]=%a" o_j e1 o_j e2 o_j e3
  | JPre(op,e1) -> Format.fprintf fp "(%s %a)" op o_j e1
  | JApp(e,es) -> Format.fprintf fp "%a(%a)" o_j e o_js es
  | JIf(e1,e2,e3) -> Format.fprintf fp "(%a ? %a : %a)" o_j e1 o_j e2 o_j e3
  | JFun(is, s) -> Format.fprintf fp "function(%a){%a}" o_is is o_s s
  | JRec(ies) -> Format.fprintf fp "{%a}" o_ijs ies
and o_js fp js =
  Format.fprintf fp "%a@?" (os o_j) js
and o_ij fp (i,j) =
  Format.fprintf fp "\"%s\":%a" i o_j j
and o_ijs fp js =
  Format.fprintf fp "%a@?" (os o_ij) js
and o_s fp = function
  | SVar(i,j) -> Format.fprintf fp "var %s = %a;" i o_j j
  | SDef(i) -> Format.fprintf fp "var %s;" i
  | SAssign(i,j) -> Format.fprintf fp "%s = %a;" i o_j j
  | SExp(j) -> Format.fprintf fp "%a;" o_j j
  | SRet(j) -> Format.fprintf fp "return %a;" o_j j
  | SIf(j,s1, s2) -> Format.fprintf fp "if(%a) %a else %a" o_j j o_s2 s1 o_s2 s2
  | SFun(i,is, s) -> Format.fprintf fp "function %s(%a) {%a}" i o_is is o_s s
  | SCons(s1,s2) -> Format.fprintf fp "%a %a" o_s s1 o_s s2
  | SNil -> ()
and o_s2 fp = function
  | (SCons(_,_)) as e -> Format.fprintf fp "{%a}" o_s e
  | e -> o_s fp e

let to_js oc e =
  Syntax.counter := 0;
  let a = Alpha.f e in
  (*Format.fprintf Format.std_formatter "//a=%a@." show_e a;*)
  let b = Javascript.f a in
  (*Format.fprintf Format.std_formatter "//b=%a@." p_s b;*)
  Format.fprintf oc "(function(){@.%a@.}());@." o_s b

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
  Format.fprintf (Format.formatter_of_out_channel oc) "%a\n" to_js (To_if.f ast)
