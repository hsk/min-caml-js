type e =
| Int of int
| Bin of e * string * e
| Var of string list
| Let of string * e * e
| Unit
| LetRec of string * string list * e * e
| Closure of env * string list * e
| App of e * e list
| NFun of (env * e list -> env * e)
| Open of string
| Module of env
| Mod of string * es
and env = (string * e) list
and es = e list
and ss = string list
[@@deriving show]

module FileSystem = struct
  let files = [
    "test.ml",[
      Int 1;
      Bin (Int 1,"+", Int 2);
      Let("a",Int 1, Unit);
      Var["a"];
      Let("_",Let("a",Int 11,Var["a"]), Unit);
      LetRec("f",["a";"b"],Bin(Var["a"],"+",Var["b"]), Unit);
      App(Var["f"],[Int 100;Int 23]);
      LetRec("print_int", ["a"], App(NFun(function
        | (env,[Int a]) ->
          print_int a;
          (env, Unit)
        | _ -> assert false
      ),[Var["a"]]), Unit);
      Let("_",App(Var["print_int"],[Int 1]), Unit);
      LetRec("print_newline", [], App(NFun(function
        | (env,_) -> print_newline(); (env,Unit)
      ),[]), Unit);

      Let("_",App(Var["print_int"],[Int 1]),
        App(Var["print_newline"],[]));
    ];
  ]
  let read f = List.assoc f files
end

let rec eval (env:(string * e)list) = function
  | Int i -> (env, Int i)
  | Bin(a,"+",b) ->
    let (env,a) = eval env a in
    let (env,b) = eval env b in
    begin match (a,b) with
    | Int a, Int b -> (env, Int(a + b))
    | _ -> assert false
    end
  | Let(i,e1,e2) ->
    let (_,e1) = eval env e1 in
    let env = (i,e1)::env in
    eval env e2
  | Unit -> (env, Unit)
  | Var[i] ->
      (env, List.assoc i env)
  | LetRec(a,es,e1,e2) ->
    let env = (a,Closure(env,es,e1))::env in
    eval env e2
  | App(e1,es) ->
    let (env, e1) = eval env e1 in
    let (env, es) = List.fold_left (fun (env,es) e ->
      let (env,e) = eval env e in
      (env, e::es)
    ) (env,[]) es in
    let es = List.rev es in
    begin match e1 with
      | Closure(cenv,is,e) ->
        let cenv = List.fold_left2 (fun env i e ->
          (i,e)::env
        ) cenv is es in
        let (_,e) = eval cenv e in
        (env,e)
      | NFun(f) -> f (env, es)
      | _ -> assert false
    end
  | NFun(_) as e -> (env,e)
  | e ->
    assert false

let rec evals env = function
  | [] -> env
  | x::xs ->
    Format.printf "eval : %s\n" (show_e x);
    let (env, x) = eval env x in
    Format.printf "- : %s\n" (show_e x);
    evals env xs

let _ =
  let env = [] in
  let env = evals env (FileSystem.read "test.ml") in
  env
