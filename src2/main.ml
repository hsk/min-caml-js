let lexbuf outchan str =
  (*Printf.printf "%s\n" str;*)
  Syntax.counter := 0;
  let ast = Parser.parse str in
  (*Format.printf "%a@." show_e ast;*)
  Emit.f outchan (ast)

let read_all inchan =
  let lines = ref [] in
  try 
    while true; do
      lines := input_line inchan :: !lines
    done;
    ""
  with
    | e -> String.concat "\n" (List.rev !lines)

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".js") in
  try
    lexbuf outchan (read_all inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := !files @ [s])
    ("Min-Caml-JS Compiler\n" ^
     Printf.sprintf "usage: %s ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
