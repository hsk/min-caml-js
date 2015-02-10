let a = ref 5 in
let b = !a in
print_int b;
a := b + 1;
print_int(!a);
a := !a + 1;
print_int(!a);
print_newline()