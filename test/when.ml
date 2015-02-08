type a =
| A of int * a
| B of int
| C
;;
let a = match 3 with
	| a when a = 1 -> 2
	| 2 when true -> 3
	| b -> b
in
let b = match A(1,B(2)) with
  | C when true -> 100
  | B(b) when b = 5 -> 200
  | A(a,B(b)) when b = 2 -> a + b
  | _ -> 50 
in print_int (a+b)
