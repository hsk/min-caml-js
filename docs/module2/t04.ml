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

type r =
  | RUnit
  | RLet of string * r * r
and ss = string list
[@@deriving show]

let rec eval = function
  | Unit -> RUnit
  | Open(x, e2) ->
    let e1 = FS.read x in
    let r1 = eval e1 in
    let r2 = eval e2 in
    RLet(x, r1, r2)
  
and start x =
  let e = (FS.read x) in
  Format.printf "    %s\n" (show_e e);
  let r = eval e in
  RLet(x, r, RUnit)

let test file =
  Format.printf "## %s\n" file;
  let r = start file in
  Format.printf "    %s\n" (show_r r)

let _ =
  List.iter(fun (file,_) ->
    test file
  ) FS.files
