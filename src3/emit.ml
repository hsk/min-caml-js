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
  | JVar("String.length") -> Format.fprintf fp "String.length_"
  | JVar(s) -> Format.fprintf fp "%s" s
  | JStr(s) -> Format.fprintf fp "\"%s\"" s
  | JBool(i) -> Format.fprintf fp "%b" i
  | JRaise(s) -> Format.fprintf fp "(function(){throw \"%s\";}())" s
  | JUndefined -> Format.fprintf fp "undefined"
  | JBin(e1,op,e2) -> Format.fprintf fp "(%a %s %a)" o_j e1 op o_j e2
  | JGet(e1,JStr e2) -> Format.fprintf fp "%a.%s" o_j e1 e2
  | JGet(e1,e2) -> Format.fprintf fp "%a[%a]" o_j e1 o_j e2
  | JPut(e1,JStr e2,e3) -> Format.fprintf fp "%a.%s=%a" o_j e1 e2 o_j e3
  | JPut(e1,e2,e3) -> Format.fprintf fp "%a[%a]=%a" o_j e1 o_j e2 o_j e3
  | JPre(op,e1) -> Format.fprintf fp "(%s %a)" op o_j e1
  | JApp(e,[JUndefined]) -> Format.fprintf fp "%a()" o_j e
  | JApp(e,es) -> Format.fprintf fp "%a(%a)" o_j e o_js es
  | JIf(e1,e2,e3) -> Format.fprintf fp "(%a ? %a : %a)" o_j e1 o_j e2 o_j e3
  | JFun(is, s) -> Format.fprintf fp "(function(%a){%a})" o_is is o_s s
  | JRec(ies) -> Format.fprintf fp "{%a}" o_ijs ies

and o_js fp js =
  Format.fprintf fp "%a@?" (os o_j) js

and o_ij fp (i,j) =
  Format.fprintf fp "\"%s\":%a" i o_j j

and o_ijs fp js =
  Format.fprintf fp "%a@?" (os o_ij) js

and o_s fp = function
  | SVar(i,JVar j) when i = j -> ()
  | SVar("",JVar j) -> ()
  | SVar("",j) -> Format.fprintf fp "%a;" o_j j
  | SVar(i,j) -> Format.fprintf fp "var %s = %a;" i o_j j
  | SDef("") -> ()
  | SDef(i) -> Format.fprintf fp "var %s;" i
  | SAssign(i,JVar j) when i = j -> ()
  | SAssign("",JVar j) -> ()
  | SAssign("",j) -> Format.fprintf fp "%a;" o_j j
  | SAssign(i,j) -> Format.fprintf fp "%s = %a;" i o_j j
  | SExp(j) -> Format.fprintf fp "%a;" o_j j
  | SRet(JUndefined) -> Format.fprintf fp "return;"
  | SRet(j) -> Format.fprintf fp "return %a;" o_j j
  | SIf(j,s1, s2) -> Format.fprintf fp "if(%a) %a else %a" o_j j o_s2 s1 o_s2 s2
  | SFun(i,is, s) -> Format.fprintf fp "function %s(%a) {%a}" i o_is is o_s s
  | SCons(s1,s2) -> Format.fprintf fp "%a %a" o_s s1 o_s s2
  | SNil -> ()

and o_s2 fp = function
  | (SCons(_,_)) as e -> Format.fprintf fp "{%a}" o_s e
  | e -> o_s fp e

let to_js oc e =
  let a = Alpha.f e in
  let a = Inline.f e in
  (*Format.fprintf Format.std_formatter "//a=%a@." show_e a;*)
  let b = Javascript.f a in
  (*Format.fprintf Format.std_formatter "//b=%a@." p_s b;*)
  Format.fprintf oc "(function(){@.%a@.}());@." o_s b

let f oc ast =


  Format.eprintf "generating javascript...@.";
  Printf.fprintf oc "%s\n" (Syntax.read_all "../libs/lib.js");
  Format.fprintf (Format.formatter_of_out_channel oc) "%a\n" to_js (To_if.f ast)
