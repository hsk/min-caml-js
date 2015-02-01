type e =
| EUnit
| EInt of int
| EAdd of e * e
;;
let rec eval e =
  begin match e with
    | EUnit -> 0
    | EInt(a) -> a
    | EAdd(e1,e2) -> eval e1 + eval e2
  end
in print_int(eval (EAdd(EUnit, EInt 2)))
