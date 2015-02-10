let a = (1,(2,3)) in
let c = match a with | (a, (b,c as d)) -> (a+b+c, d) in
let d = match c with (a,(b,c)) -> a + b + c in
print_int(d); print_newline()
