let a = ref 5 in
let b = !a in
print_int b;
a := b + 1;
print_int(!a);
a := !a + 1;
print_int(!a);
print_newline();
let rec f a1 =
	a := 1000;
	a1
in print_int(f !a);
print_newline()
