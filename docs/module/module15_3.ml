type e =
| Unit
| Open of string
| Mod of string * es
and v =
| Env of env 
and env = (string * v) list
and es = e list
and ss = string list
[@@deriving show]

module FileSystem = struct
  let files = [
    "A",[
    ];
    "E", [
      Mod ("Inner",[
        Mod ("Inner2",[
        ]);
      ]);
      Open "A";
      Open "Inner";
      Open "Inner2";
    ];
    "F", [
      Open "A";
      Open "E";
      Open "A";
      Open "E";
    ];
  ]
  let read f = List.assoc f files
end

let rec eval (env:env) = function
  | Unit -> (env, Unit)
  | Open(x) ->

    if List.mem_assoc x env then (
      let Env env2 = (List.assoc x env) in
      (env2 @ env,Unit)
    )
    else (
      Format.printf "read %s\n" x;

      let(env,result) = List.fold_left (fun (env,r) e ->
        let (env, result) = eval env e in
        (env, result)
      ) (env,Unit) (FileSystem.read x) in
      ((x,Env env)::env, result)
    )
  | Mod(s, es) ->
    let envm = evals env es in
    ((s,Env envm)::envm, Unit) 

and evals env = function
  | [] -> env
  | x::xs ->
    Format.printf "eval : %s\n" (show_e x);
    let (env, x) = eval env x in
    Format.printf "- : %s\n" (show_e x);
    evals env xs

let _ =
  let env = [] in
  let env = evals env (FileSystem.read "F") in
  env

