type a =
| A of int * int
| B of int
| C
;;
let a = match 3 with
	| a when a = 1 -> 2
	| 2 when true -> 3
	| b -> b
in
let b = match A(1,2) with
  | C when true -> 100
  | B(b) when b = 5 -> 200
  | A(a,b) when a = 1 -> 1000
  | A(a,b) -> 100
  | _ -> 50 
in print_int (a+b)