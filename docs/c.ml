(*
todo: これは、たぶん、javascriptに変換する話のサンプルコードだけど関連が謎

*)
type e =
  | EInt of int
  | EVar of string
  | EAdd of e * e
  | EApp of e * e list
  | EIf of e * e * e
  | ELet of string * e * e
  | ELetRec of string * string list * e * e
  | EUnit

type j =
  | JInt of int
  | JVar of string
  | JAdd of j * j
  | JApp of j * j list
  | JIf of j * j * j
  | JFun of string list * s
  | JUndefined
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
let rec p_e fp = function
  | EInt(i) -> Format.fprintf fp "EInt(%d)" i
  | EVar(s) -> Format.fprintf fp "EVar(\"%s\")" s
  | EUnit -> Format.fprintf fp "EUnit"
  | EAdd(e1,e2) -> Format.fprintf fp "EAdd(%a, %a)" p_e e1 p_e e2
  | EApp(e,es) -> Format.fprintf fp "EApp(%a, %a)" p_e e p_es es
  | EIf(e1,e2,e3) -> Format.fprintf fp "EIf(%a, %a, %a)" p_e e1 p_e e2 p_e e3
  | ELet(s,e1,e2) -> Format.fprintf fp "ELet(%s, %a, %a)" s p_e e1 p_e e2
  | ELetRec(s, ss, e1, e2) -> Format.fprintf fp "ELetRec(%s, %a, %a, %a)" s p_is ss p_e e1 p_e e2
and p_es fp es =
  Format.fprintf fp "[%a]@?" (ps p_e) es

let rec p_j fp = function
  | JInt(i) -> Format.fprintf fp "JInt(%d)" i
  | JVar(s) -> Format.fprintf fp "JVar(\"%s\")" s
  | JUndefined -> Format.fprintf fp "JUndefined"
  | JAdd(e1,e2) -> Format.fprintf fp "JAdd(%a, %a)" p_j e1 p_j e2
  | JApp(e,es) -> Format.fprintf fp "JApp(%a, %a)" p_j e p_js es
  | JIf(e1,e2,e3) -> Format.fprintf fp "JIf(%a, %a, %a)" p_j e1 p_j e2 p_j e3
  | JFun(is, s) -> Format.fprintf fp "JFun(%a, %a)" p_is is p_s s
and p_js fp js =
  Format.fprintf fp "[%a]@?" (ps p_j) js
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
  | JVar(s) -> Format.fprintf fp "%s" s
  | JUndefined -> Format.fprintf fp "undefined"
  | JAdd(e1,e2) -> Format.fprintf fp "(%a + %a)" o_j e1 o_j e2
  | JApp(e,es) -> Format.fprintf fp "%a(%a)" o_j e o_js es
  | JIf(e1,e2,e3) -> Format.fprintf fp "(%a ? %a : %a)" o_j e1 o_j e2 o_j e3
  | JFun(is, s) -> Format.fprintf fp "function(%a){%a}" o_is is o_s s
and o_js fp js =
  Format.fprintf fp "%a@?" (os o_j) js
and o_s fp = function
  | SVar(i,j) -> Format.fprintf fp "var %s = %a;" i o_j j
  | SDef(i) -> Format.fprintf fp "var %s;" i
  | SAssign(i,j) -> Format.fprintf fp "%s = %a;" i o_j j
  | SExp(j) -> Format.fprintf fp "%a;" o_j j
  | SRet(j) -> Format.fprintf fp "return %a;" o_j j
  | SIf(j,s1, s2) -> Format.fprintf fp "if(%a) {%a} else {%a}" o_j j o_s s1 o_s s2
  | SFun(i,is, s) -> Format.fprintf fp "function %s(%a) {%a}" i o_is is o_s s
  | SCons(s1,s2) -> Format.fprintf fp "%a %a" o_s s1 o_s s2
  | SNil -> ()


let counter = ref 0
let genid s =
  counter := !counter + 1;
  s ^ "_" ^ string_of_int !counter

let find env i = if List.mem_assoc i env then List.assoc i env else i
let mem env i = List.mem_assoc i env
let rec alpha env = function
  | EInt(i) -> EInt(i)
  | EVar(s) -> EVar(find env s)
  | EUnit -> EUnit
  | EAdd(e1,e2) -> EAdd(alpha env e1, alpha env e2)
  | EApp(e,es) -> EApp(alpha env e, List.map (alpha env) es)
  | EIf(e1,e2,e3) -> EIf(alpha env e1, alpha env e2, alpha env e3)
  | ELet(s,e1,e2) ->
    let s' = if mem env s then genid s else s in
    ELet(s',alpha env e1, alpha ((s,s')::env) e2)
  | ELetRec(s, ss, e1, e2) ->
    let s' = if mem env s then genid s else s in
    let env' = List.fold_left (fun env s -> (s,s)::env) env ss in
    ELetRec(s', ss, alpha ((s,s')::env') e1, alpha ((s,s')::env) e2)

let (@@) a b=
  if a = SNil then b else
  if b = SNil then a else SCons(a,b)

let rec cnve = function
  | EInt(i) -> SNil,JInt(i)
  | EVar(i) -> SNil,JVar(i)
  | EUnit -> SNil,JUndefined
  | EAdd(e1,e2) ->
    let s1,j1 = cnve e1 in
    let s2,j2 = cnve e2 in
    s1 @@ s2, JAdd(j1, j2)
  | EApp(e,es) ->
    let s,j = cnve e in
    let ss,js = List.fold_right (fun e (ls,es) ->
      let l,e = cnve e in
      l @@ ls, (e::es)
    ) es (SNil,[]) in
    s @@ ss, JApp(j, js)
  | EIf(e1,e2,e3) ->
    let s1,j1 = cnve e1 in
    let s2,j2 = cnve e2 in
    let s3,j3 = cnve e3 in
    let i = genid "tmp" in
    SDef(i) @@ s1 @@ SIf(j1, s2 @@ SAssign(i,j2), s3 @@ SAssign(i,j3)), JVar(i)
  | ELet(i,e1,e2) ->
    let s1, j1 = cnve e1 in
    let s2, j2 = cnve e2 in
    s1 @@ SVar(i, j1) @@ s2, j2
  | ELetRec(i, is, e1, e2) ->
    let s1 = cnv true e1 in
    let s2, j2 = cnve e2 in
    SFun(i, is, s1) @@ s2, j2
  | e -> Format.fprintf Format.std_formatter "error %a@." p_e e; assert false

and cnv tl = function
  | ELet(i,e1,e2) ->
    let s1, j1 = cnve e1 in
    s1 @@ SVar(i, j1) @@ cnv tl e2
  | ELetRec(i, is, e1, e2) ->
    let s1 = cnv true e1 in
    SFun(i, is, s1) @@ cnv tl e2
  | EIf(e1, e2, e3) when tl ->
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

let test e =
  counter := 0;
  let a = alpha [] e in
  Format.fprintf Format.std_formatter "//a=%a@." p_e a;
  let b = cnv false a in
  Format.fprintf Format.std_formatter "//b=%a@." p_s b;
  Format.fprintf Format.std_formatter "(function(){@.%a@.}());@." o_s b

let _ = 
  (*

  let rec f x y =
    let a = x + 10 in
    let b = y + 10 in
    a + b
  in ()
  *)
  let src = ELetRec("f", ["a"; "b"],
    ELet("a", EAdd(EVar "a", EInt 10),
    ELet("b", EAdd(EVar "b", EInt 10),
    EAdd(EVar "a", EVar "b")
    )), EApp(EVar("console.log"),[EApp(EVar("f"),[EInt 1; EInt 2])]))
  in
  test src;
  let src = ELetRec("f", ["x"; "b"],
    ELet("a", EAdd(EVar "x", EInt 10),
    ELet("b", EAdd(EVar "b", EInt 10),
    EAdd(EVar "a", EVar "b")
    )), EApp(EVar("console.log"),[EApp(EVar("f"),[EInt 1; EInt 2])]))
  in
  test src;
  let src =
    ELetRec("f", ["x"],
      EAdd(EInt 100,EIf(EVar "x",
        EIf(EInt 10,EVar "x",EInt 222),
        EAdd(EInt 20,EVar "x")
      )),
    EApp(EVar("console.log"),[EApp(EVar("f"),[EInt 1])]))
  in
  test src;
  let src =
    ELetRec("f", ["x"],
      EIf(EVar "x",
        EIf(EInt 10,EVar "x",EInt 222),
        EAdd(EInt 20,EVar "x")
      ),
    EApp(EVar("console.log"),[EApp(EVar("f"),[EInt 1])]))
  in
  test src;
  let src = EApp(EVar("console.log"),[
    ELet("a", EInt 1, EVar "a")
  ])
  in
  test src;
  let src = EApp(EVar("console.log"),[
    ELetRec("a", ["v"], EVar "v", EApp(EVar "a",[EInt 55]))
  ])
  in
  test src;

  let src = EApp(EVar("console.log"),[
    ELetRec("a", ["v"], EAdd(EIf(EVar "v", EInt 10, EInt 20),EInt 1), EApp(EVar "a",[EInt 55]))
  ])
  in
  test src;

  ()
