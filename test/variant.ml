type e =
| EInt of int
| EAdd of e * e
| T_1
;;
let rec eval e =
  begin match e with
  	| T_1 -> 1
    | EInt(a) -> a
    | EAdd(e1,e2) -> eval e1 + eval e2
  end
in print_int(eval (EAdd(T_1, EInt 2)))
