type e =
  | S of string
  | Ls of e list

let read_all file =
  let inchan = open_in file in
  let lines = ref [] in
  let rc =
    begin try 
      while true; do
        lines := input_line inchan :: !lines
      done;
      ""
    with
      | e -> String.concat "\n" (List.rev !lines)
    end
  in
  close_in inchan;
  rc
