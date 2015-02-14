type e =
  | Unit
  | Open of string * e
[@@deriving show]

module FS = struct
  let files = [
    "A",
      Unit;
    "E",
      Open("A", Unit);
    "F", 
      Open("E", Unit);
    "Inner",
      Open("F", Unit);
    "G",
      Open("Inner", Open("E", Open("F", Unit)));
    "H", Open("I", Unit);
    "I", Open("H", Unit);
    "J", Open("K", Unit);
    "K", Open("L", Unit);
    "L", Open("J", Unit);

  ]
  let read f = List.assoc f files
end

type ss = string list
[@@deriving show]

let rec eval = function
  | Unit -> []
  | Open(x,e) -> x::(eval e)

let test file =
  let e = FS.read file in
  Format.printf "## %s\n" file;
  Format.printf "    %s\n" (show_e e);
  let ss = eval e in
  Format.printf "    %s\n" (show_ss ss)

let _ =
  List.iter(fun (file,_) ->
    test file
  ) FS.files
