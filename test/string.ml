let a = "aiu\n\t\"\n" in
print_string a;
print_string (String.sub "ABC" 1 1);
print_string (String.sub "ABC" 0 3);
print_int (String.length "ABC");
print_string (String.concat ";" ["a";"b";"c"]);
print_newline()
