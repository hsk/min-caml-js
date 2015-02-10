type r = {a:int;mutable b:int}
;;
let a ={a=1;b=20} in

print_int(a.a);print_newline();
print_int(a.b);print_newline();

let {a=k} = a in
print_int(k);print_newline();
begin match a with
| {b=b} -> print_int(b); print_newline()
end;
a.b <- 40;
print_int(a.b);
print_newline()
