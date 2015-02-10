let rec hd ls = match ls with x::_ -> x | [] -> assert false in
let rec tl ls = match ls with _::xs -> xs | [] -> assert false in
let rec concat xs ys =
  match xs with
  | [] -> ys
  | x::xs -> x::(concat xs ys)
in
let ls = [(let a = 1 in a);2;3] @ [4;5] @ [6] in
print_int(hd ls);print_newline();
print_int(hd (tl ls));print_newline();
let rec sum a ls =
	match ls with
	| [] -> a
	| x::xs -> sum (a+x) xs
in
print_int(sum 0 ls);print_newline()
