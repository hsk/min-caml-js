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
  | JFun of string list * s list
  | JUndefined
and s =
  | SVar of string * j
  | SDef of string
  | SAssign of string * j
  | SExp of j
  | SIf of j * s list * s list
  | SFun of string * string list * s list
  | SRet of j
type p = s list

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
  | JFun(is, ss) -> Format.fprintf fp "JFun(%a, %a)" p_is is p_ss ss
and p_js fp js =
  Format.fprintf fp "[%a]@?" (ps p_j) js
and p_s fp = function
  | SVar(i,j) -> Format.fprintf fp "SVar(\"%s\",%a)" i p_j j
  | SDef(i) -> Format.fprintf fp "SDef(\"%s\")" i
  | SAssign(i,j) -> Format.fprintf fp "SAssign(\"%s\",%a)" i p_j j
  | SExp(j) -> Format.fprintf fp "SExp(%a)" p_j j
  | SRet(j) -> Format.fprintf fp "SRet(%a)" p_j j
  | SIf(j,ss1, ss2) -> Format.fprintf fp "SIf(%a, %a, %a)" p_j j p_ss ss1 p_ss ss2
  | SFun(i,is, ss) -> Format.fprintf fp "SFun(%a, %a, %a)" p_i i p_is is p_ss ss
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
  | JFun(is, ss) -> Format.fprintf fp "function(%a){%a}" o_is is o_ss ss
and o_js fp js =
  Format.fprintf fp "%a@?" (os o_j) js
and o_s fp = function
  | SVar(i,j) -> Format.fprintf fp "var %s = %a;" i o_j j
  | SDef(i) -> Format.fprintf fp "var %s;" i
  | SAssign(i,j) -> Format.fprintf fp "%s = %a;" i o_j j
  | SExp(j) -> Format.fprintf fp "%a;" o_j j
  | SRet(j) -> Format.fprintf fp "return %a;" o_j j
  | SIf(j,ss1, ss2) -> Format.fprintf fp "if(%a) {%a} else {%a}" o_j j o_ss ss1 o_ss ss2
  | SFun(i,is, ss) -> Format.fprintf fp "function %s(%a) {%a}" i o_is is o_ss ss
and o_ss fp ss =
  Format.fprintf fp "%a@?" (ns o_s) ss


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

let rec cnve = function
  | EInt(i) -> [],JInt(i)
  | EVar(s) -> [],JVar(s)
  | EUnit -> [],JUndefined
  | EAdd(e1,e2) ->
    let l1,e1 = cnve e1 in
    let l2,e2 = cnve e2 in
    l1 @ l2, JAdd(e1, e2)
  | EApp(e,es) ->
    let l,e = cnve e in
    let ls,es = List.fold_right (fun  e (ls,es) ->
      let l,e = cnve e in
      (l@ls), (e::es)
    ) es ([],[]) in
    (l @ ls), JApp(e, es)
  | EIf(e1,e2,e3) ->
    let s = genid "tmp" in
    let l1,e1 = cnve e1 in
    let l2,e2 = cnve e2 in
    let l3,e3 = cnve e3 in
    (SDef(s):: l1 @ [SIf(e1, l2 @ [SAssign(s,e2)], l3 @ [SAssign(s,e3)])]) , JVar(s)
  | e -> Format.fprintf Format.std_formatter "error %a@." p_e e; assert false

and cnv tl = function
  | ELet(s,e1,e2) ->
    let l1,e1 = cnve e1 in
    l1 @ SVar(s, e1) :: cnv tl e2
  | ELetRec(s, ss, e1, e2) ->
    let l1 = cnv true e1 in
    SFun(s,ss, l1) :: cnv tl e2
  | EIf(e1,e2,e3) when tl ->
    let l1,e1 = cnve e1 in
    let e2 = cnv true e2 in
    let e3 = cnv true e3 in
    l1 @ [SIf(e1, e2, e3)]
  | e when tl ->
    let l,e = cnve e in
    l @ [SRet e]
  | e ->
    let l,e = cnve e in
    l @ [SExp e]

let test e =
  counter := 0;
  let a = alpha [] e in
  Format.fprintf Format.std_formatter "//a=%a@." p_e a;
  let b = cnv false a in
  Format.fprintf Format.std_formatter "//b=%a@." p_ss b;
  Format.fprintf Format.std_formatter "(function(){@.%a@.}());@." o_ss b

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
  ()
