type t = string

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "__mincaml__tmp__%s__%d" s !counter

let gentmp () =
  incr counter;
  Printf.sprintf "__mincaml__tmp__%d" !counter
