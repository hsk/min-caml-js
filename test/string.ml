let a = "aiu\n\t\"\n" in
print_string a;
print_string (String.sub "ABC" 1 1);
print_string (String.sub "ABC" 0 3);
print_int (String.length "ABC");
print_string (String.concat ";" ["a";"b";"c"]);
print_string (String.capitalize "abcd");
print_string (String.uncapitalize "Abcd");
print_int (int_of_char(String.get "123" 0));
print_newline();

Printf.printf "a\n";

Printf.printf "%s\n" "hoge";
Printf.printf "%d\n" 111;

Printf.printf "%f\n" 111.0;

Printf.printf "%f\n" 111.123456;

let rec f1 oc e =
  Format.fprintf oc "%f\n" e
in
Format.printf "%a@." f1 1.23;
let s = (Format.sprintf "%f@." 1.23) in
Format.printf "%s@." s
