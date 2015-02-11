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

type r =
| RRet
| RExp
| RVar of string

let rec cnve tl = function
  | Int(i) -> SNil,JInt(i)
  | Float(i) -> SNil,JFloat(i)
  | Var(i) -> SNil,JVar(i)
  | Str(i) -> SNil,JStr(i)
  | Raise(i) -> SNil,JRaise(i)
  | Bool(i) -> SNil,JBool(i)
  | Unit -> SNil,JUndefined
  | Bin(e1,op,e2) ->
    let s1,j1 = cnve tl e1 in
    let s2,j2 = cnve tl e2 in
    s1 @@ s2, JBin(j1, op, j2)
  | Get(e1,e2) ->
    let s1,j1 = cnve tl e1 in
    let s2,j2 = cnve tl e2 in
    s1 @@ s2, JGet(j1, j2)
  | Put(e1,e2,e3) ->
    let s1,j1 = cnve tl e1 in
    let s2,j2 = cnve tl e2 in
    let s3,j3 = cnve tl e3 in
    s1 @@ s2 @@ s3, JPut(j1, j2, j3)
  | Pre(op,e1) ->
    let s1,j1 = cnve tl e1 in
    s1, JPre(op, j1)
  | App(e,es) ->
    let s,j = cnve tl e in
    let ss,js = List.fold_right (fun e (ls,es) ->
      let l,e = cnve tl e in
      l @@ ls, (e::es)
    ) es (SNil,[]) in
    s @@ ss, JApp(j, js)
  | Let(i,e1,e2) ->
    let s1, j1 = cnve tl e1 in
    let s2, j2 = cnve tl e2 in
    s1 @@ SVar(i, j1) @@ s2, j2
  | LetRec(i, Fun(is, e1), e2) ->
    let s1 = cnv RRet e1 in
    let s2, j2 = cnve tl e2 in
    SFun(i, is, s1) @@ s2, j2

  | Fun(is, e1) ->
    let s1 = cnv RRet e1 in
    SNil, JFun(is, s1)
  | Rec(ies) ->

    let ss,js = List.fold_right (fun (i,e) (ls,es) ->
      let l,e = cnve tl e in
      l @@ ls, ((i,e)::es)
    ) ies (SNil,[]) in
    ss,JRec(js)
  | If(e1,e2,e3) ->
    begin match tl with
    | RVar i ->
      let s1,j1 = cnve RExp e1 in
      let s2,j2 = cnve tl e2 in
      let s3,j3 = cnve tl e3 in
      s1 @@ SIf(j1, s2 @@ SAssign(i,j2), s3 @@ SAssign(i,j3)), JVar(i)
    | _ ->
      let s1,j1 = cnve RExp e1 in
      let s2,j2 = cnve tl e2 in
      let s3,j3 = cnve tl e3 in
      let i = gentmp () in
      SDef(i) @@ s1 @@ SIf(j1, s2 @@ SAssign(i,j2), s3 @@ SAssign(i,j3)), JVar(i)
    end
  | e -> Format.fprintf Format.std_formatter "error %a@." show_e e; assert false

and cnv tl = function
  | Let(i,e1,e2) ->
    let s1, j1 = cnve (RVar i) e1 in
    begin match s1 with
    | SNil -> SVar(i, j1) @@ cnv tl e2
    | _ -> SDef(i) @@ s1 @@ SAssign(i, j1) @@ cnv tl e2
    end
  | LetRec(i, Fun(is, e1), e2) ->
    let s1 = cnv RRet e1 in
    SFun(i, is, s1) @@ cnv tl e2
  | If(e1, e2, e3) when tl=RRet ->
    let s1, j1 = cnve RExp e1 in
    let s2 = cnv RRet e2 in
    let s3 = cnv RRet e3 in
    s1 @@ SIf(j1, s2, s3)
  | e ->
    let s, j = cnve RExp e in
    s @@ (match tl with RRet -> SRet j | _ -> SExp j)

let f e = cnv RExp e
