let rec hd ls = match ls with x::_ -> x | [] -> assert false in
let ls = 1::[]  in
print_int(hd ls);print_newline()