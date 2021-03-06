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
    "prelude.ml", [
      LetRec("print_int", ["a"], App(NFun(function
        | (env,[Int a]) ->
          print_int a;
          (env, Unit)
        | _ -> assert false
      ),[Var["a"]]), Unit);
      LetRec("print_newline", [], App(NFun(function
        | (env,_) -> print_newline(); (env,Unit)
      ),[]), Unit);
    ];

    "a.ml",[
      LetRec("print_int_ln", ["a"],
        Let("_",
          App(Var["print_int"], [Var["a"]]),
          App(Var["print_newline"],[])),
      Unit);

      App (Var["print_int_ln"], [Int 1]);
    ];
    "b.ml", [
      Open "A";

      Let("_",
        App(Var["print_int_ln"],[Int 12345]),
      Unit);
    ];
    "c.ml", [

      Let("_",
        App(Var["A";"print_int_ln"],[Int 12345]),
      Unit);
      Let("k", Int 55555, Unit);
    ];
    "d.ml", [
      Open "C";
      Let("_",
        App(Var["A";"print_int_ln"],[Var["k"]]),
      Unit);

      Let("_",
        App(Var["A";"print_int_ln"],[Var["C";"k"]]),
      Unit);
    ];
    "e.ml", [
      Mod ("Inner",[
        Let("k", Int 55555, Unit);
      ]);
      Let("_",
        App(Var["A";"print_int_ln"],[Var["Inner";"k"]]),
      Unit);
    ];
    "f.ml", [
      Let("_",
        App(Var["A";"print_int_ln"],[Var["E";"Inner";"k"]]),
      Unit);
      Let("_",
        App(Var["A";"print_int_ln"],[Var["E";"Inner";"k"]]),
      Unit);
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
    let (env1,e1) = eval env e1 in
    let modules = List.filter(fun (n,v)->
      let c = String.get n 0 in
      (c >= 'A' && c <= 'Z')
    ) env1 in
    let env = (i,e1)::(modules @ env) in
    eval env e2
  | Unit -> (env, Unit)
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
  | Var i ->
    begin try
      let rec findE (env:(string * e)list) = function
        | [] -> assert false
        | x::xs ->
          begin match List.assoc x env with
          | Module(env) -> findE env xs
          | e -> e
        end
      in
      (env, findE env i)
    with
      | _ ->
        begin match i with
        | x::xs when not (List.mem_assoc x env) ->
          let (env2,_) = eval env (Open x) in
          eval env2 (Var i)
        | _ ->
          Format.printf "not found variable %a\n" pp_ss i;
          assert false
        end
    end
  | Open(x) ->

    if List.mem_assoc x env then (
      match (List.assoc x env) with
      | Module(env2) -> (env2 @ env,Unit)
      | _ -> Format.printf "open error %s\n" x; assert false
    )
    else (
      let filename = (String.uncapitalize x) ^ ".ml" in
      Format.printf "read %s\n" filename;

      let(env,result) = List.fold_left (fun (env,r) e ->
        let (env, result) = eval env e in
        (env, result)
      ) (env,Unit) (FileSystem.read filename) in
      ((x,Module env)::env, result)
    )
  | Mod(s, es) ->
    let envm = evals env es in
    ((s,Module(envm))::envm, Unit) 
  | e ->
    assert false

and evals env = function
  | [] -> env
  | x::xs ->
    Format.printf "eval : %s\n" (show_e x);
    let (env, x) = eval env x in
    Format.printf "- : %s\n" (show_e x);
    evals env xs

let rec evals1 env = function
  | [] -> env
  | x::xs ->
    let (env, x) = eval env x in
    evals1 env xs

let _ =
  let env = [] in
  let env = evals1 env (FileSystem.read "prelude.ml") in
  let env = evals env (FileSystem.read "f.ml") in
  env
