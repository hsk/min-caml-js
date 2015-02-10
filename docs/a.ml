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
  | SFun of string * string list * s list
  | SRet of j
type p = s list

(*

let rec f x y =
	let a = x + 10 in
	let b = y + 10 in
	a + b
in ()
*)
let src = ELetRec("f", ["x"; "y"],
	ELet("a", EAdd(EVar "x", EInt 10),
	ELet("b", EAdd(EVar "y", EInt 10),
	EAdd(EVar "a", EVar "b")
	)), EUnit)

(*
	(function(){
	  function f(x, y) {
	    return (function(){var a = x * 10;
	    return (function(){var b = y * 10;
	    return a + b;
	    }());
	    }());
	  }
	  return undefined;
	})
*)

let dst = JApp
 (JFun ([],
   [SFun ("f", ["x"; "y"],
     [SRet
       (JApp
         (JFun ([],
           [SVar ("a", JAdd (JVar "x", JInt 10));
            SRet
             (JApp
               (JFun ([],
                 [SVar ("b", JAdd (JVar "y", JInt 10));
                  SRet (JAdd (JVar "a", JVar "b"))]),
               []))]),
         []))]);
    SRet JUndefined]),
 [])


(*
n -> n
i -> i
() -> undefined
e -> e'
e1 + e2 -> (e1' + e2')
e e1 ... en -> e'(e1', ..., en')
if e1 then e2 else e3 -> (e1' ? e2' : e3')
let i = e1 in e2 -> (function(){var i = e1'; return e2';}())
let rec i i1 ... in = e1 in e2 -> (function(){function i(i1,...,in) { return e1'; } return e2'; }())
*)

let rec cnv = function
  | EInt(i) -> JInt(i)
  | EVar(s) -> JVar(s)
  | EUnit -> JUndefined
  | EAdd(e1,e2) -> JAdd(cnv e1, cnv e2)
  | EApp(e,es) -> JApp(cnv e, List.map cnv es)
  | EIf(e1,e2,e3) -> JIf(cnv e1, cnv e2, cnv e3)
  | ELet(s,e1,e2) -> JApp(JFun([],[SVar(s, cnv e1); SRet(cnv e2)]),[])
  | ELetRec(s, ss, e1, e2) -> JApp(JFun([],[SFun(s,ss,[SRet(cnv e1)]); SRet(cnv e2)]),[])

let _ =
	assert ((cnv src) = dst)
