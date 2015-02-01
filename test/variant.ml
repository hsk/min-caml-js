type e =
| EUnit
| EInt of int
| EAdd of e * e
;;
let rec eval e =
  (match e with
  	| EUnit -> 0
    | EInt(a) -> a
    | EAdd(e1,e2) -> eval e1 + eval e2
  )
in print_int(eval (EAdd(EUnit, EInt 2)))
