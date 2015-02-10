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
  | JFun(is, s) -> Format.fprintf fp "(function(%a){%a})" o_is is o_s s
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
  Printf.fprintf oc "function ref(n) { return {ref:n}; }\n";
  Printf.fprintf oc "var abs_float = Math.abs;\n";
  Printf.fprintf oc "var sqrt = Math.sqrt;\n";
  Printf.fprintf oc "var sin = Math.sin;\n";
  Printf.fprintf oc "var cos = Math.cos;\n";
  Printf.fprintf oc "var int_of_float = Math.floor;\n";
  Printf.fprintf oc "function truncate(a) { return a >= 0 ? Math.floor(a) : -Math.floor(-a); }\n";
  Printf.fprintf oc "function float_of_int(a){return a+0.0;}\n";
  Printf.fprintf oc "function print_newline(){console.log(\"\");}\n";
  Format.fprintf (Format.formatter_of_out_channel oc) "%a\n" to_js (To_if.f ast)
