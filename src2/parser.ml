open Peg

open Syntax

let keywords = [
  "let"; "in"; "if"; "else"; "then"; "true"; "false";
  "match"; "with"; "when"; "begin"; "end"; "type"; "as"; "mutable"
]

let ident i = i |> (
  (*  reg "[_a-z][_a-zA-Z0-9]*\\|\\([_A-Z][_a-zA-Z0-9]*\\.\\)+[_a-z][_a-zA-Z0-9]*"  *)

  ((rep(
    (skip >> range 'A' 'Z' <~> rep(range 'A' 'Z' <|>range 'a' 'z' <|> range '0' '9' <|> nstr "_") << str "." )
    >>>(fun (a,b)->String.concat "" (a::b) ^ "."))
  >>> (fun a -> String.concat "" a)
  ) <~>
  ((skip >> (range 'a' 'z' <|> nstr "_") <~> rep(range 'A' 'Z' <|>range 'a' 'z' <|> range '0' '9' <|> nstr "_") ) >>>(fun (a,b)->String.concat "" (a::b)))
  >>> (fun (a,b)->a^b))
  >?> (function
    | "_" -> genid ""
    | a when not(List.mem a keywords) -> a
    | _ -> failwith "error"
  )
)

let cident i = i |> (

  (*reg "[A-Z][_a-zA-Z0-9]*"*)

  ((skip >> range 'A' 'Z' <~> rep(range 'A' 'Z' <|>range 'a' 'z' <|> range '0' '9' <|> nstr "_") ) >>>(fun (a,b)->String.concat "" (a::b)))
  >?> (function
    | a when not(List.mem a keywords) -> a
    | _ -> failwith "error"
  )
)

let float_ i = i |> (
  (* reg "\\([1-9][0-9]*\\|0\\)\\.[0-9]*|\\.[0-9]+" *)
  ((skip >>
    (((range '1' '9') <~> rep(range '0' '9')>>> (fun(a,b)->String.concat "" (a::b))) <|>
    (nstr "0"))
    <~> nstr "." <~> rep(range '0' '9') >>> (fun((a,b),c)->String.concat "" (a::b::c)))
  ) <|>
  (str "." <~> rep1(range '0' '9') >>> (fun(a,b)->String.concat "" (a::b)))

) 

let int_ i = i |> (
  (*"-?\\([1-9][0-9]*\\|0\\)" *)
    ((skip >> (range '1' '9') <~> rep(range '0' '9') >>> (fun(a,b)->String.concat "" (a::b)))
     <|> (str "0"))
)

let rec str_ i = i |> (
  (skip >> nstr "\"" >> (rep((nstr "\\" <~> any_char () >>> (fun(a,b)->a^b)) <|> (notp (nstr "\"") >> any_char ())) << nstr "\"") >>> (fun a -> String.concat "" a))
)

let rec simple_exp i = i |> (
  (str "true" >>> (fun _ -> Bool true)) <|>
  (str "false" >>> (fun _ -> Bool false)) <|>
  (float_ >>> (fun a -> Float(float_of_string a))) <|>
  (int_ >>> (fun e -> Int(int_of_string e))) <|>
  (str_ >>> (fun e -> Str(e))) <|>
  (str "(" >> str ")" >>> (fun _ -> Unit)) <|>
  (str "[" >> str "]" >>> (fun _ -> CApp("Nil", Unit) )) <|>
  ((str "[" >> _let) <~> rep(str ";" >> _let) << str "]" >>> (fun (a,bs) ->
    List.fold_right (fun a b ->
      CApp("Cons", Tuple([a; b]))
    ) (a :: bs) (CApp("Nil", Unit))
  )) <|>
  (str "{" >> fields << str "}" >>> (fun a -> Rec a)) <|>
  (ident >>> (fun a -> Var a)) <|>
  (cident <~> rep1(str "." >> ident) >>> (fun (a,b) -> Var (String.concat "." (a::b) ))) <|>
  (str "begin" >> exp << str "end" >>> (fun e -> e)) <|>
  (str "(" >> exp << str ")" >>> (fun e -> e))
)
and field i = i |> (
  (ident << str "=") <~> _let >>> (fun (a,b) -> (a,b))
)
and fields i = i |> (
  ((field <~> rep(str ";" >> field)) >>> (fun (a,b) -> a::b))
)
and exp i = i |> (
  (_let <~> rep(str ";" >> _let) >>> (fun (a, bs) ->
    List.fold_left (fun a b ->
      Let(genid "", a, b)
    ) a bs
  ))
)
and _let i = i |> (

  ((str "let" >> str "rec" >> ident) <~> 
    ((str "(" >> str ")" >>> (fun a -> [] )) <|> rep(ident)) <~>
    (str "=" >> exp) <~> (str "in" >> exp)
    >>> (fun (((a,r),b),c) -> LetRec(a, Fun(r, b), c))
  ) <|>
  ((str "let" >> str "{" >> fields << str "}") <~>
    (str "=" >> exp) <~> (str "in" >> exp)
    >>> (fun ((a,b),c) -> Match(b, [(Rec(a), None, c)]))
  ) <|>
  ((str "let" >> str "(" >> exp << str ")") <~>
    (str "=" >> exp) <~> (str "in" >> exp)
    >>> (fun ((a,b),c) -> Match(b, [(a, None, c)]))
  ) <|>
  ((str "let" >> ident) <~> (str "=" >> exp) <~> (str "in" >> exp)
    >>> (fun ((a,b),c) -> Let(a, b, c))
  ) <|>
  cons
)
and cons i = i |> (
  (_if <~> opt((str "::" <|> str "@" <|> str "as") <~> cons) >>> (function
    | (a, Some("::", b)) -> CApp("Cons", Tuple([a; b]))
    | (a, Some("@", b)) -> App(Var("concat"), [a; b])
    | (a, Some(op, b)) -> Bin(a, op, b)
    | (a, None) -> a
  ))
)
and _if i = i |> (
  ((str "if" >> exp) <~> (str "then" >> exp) <~> (str "else" >> exp) >>> (function
    | (a, b), c -> If(a, b, c)
  )) <|>

  ((str "match" >> exp << str "with" << opt(str "|")) <~>
    (               exp  <~> opt(str "when" >> exp) <~> (str "->" >> exp)) <~>
    rep((str "|" >> exp) <~> opt(str "when" >> exp) <~> (str "->" >> exp))
    >>> (fun ((a,b),c) ->
      Match(a, List.map (fun ((a,b),c) -> (a, b, c)) (b :: c))
  )) <|>
  (str "type" >> ident >> str "=" >> typ >> str ";;" >> exp) <|>
  tuple
)
and types i = i |> (
  ident <~> rep(str "*" >> ident) >>> (fun (a, b) -> a :: b)
)
and tyrec  i = i |> (
  opt(str "mutable")<~>(ident << str ":") <~> typ >>> (fun a -> "")
)
and tyrecs i = i |> (
  tyrec <~> rep(str ";" >> tyrec) >>> (fun a -> "")
)
and typ i = i |> (
  (opt(str "|") >> consts >>> (fun a -> "")) <|>
  (types >>> (fun a -> "")) <|>
  (str "{" >> tyrecs << str "}" >>> (fun a -> ""))
)
and const i = i |> (
  (cident <~> (str "of" >> types) >>> (fun (a, b) -> (a, b))) <|>
  (cident >>> (fun a -> (a, [])))
)
and consts i = i |> (
  (const <~> rep(str "|" >> const) >>> (fun (a, b) -> a :: b))
)
and tuple i = i |> (
  eq <~> rep(str "," >> eq) >>> (function
    | (t1,[]) -> t1
    | (t1,t2) -> Tuple(t1 :: t2)
  )
)
and eq i = i |> (
  add <~> rep((str "=" <|> str ">=" <|> str ">" <|> str "<=" <|> str "<") <~> add)
  >>> (fun (t,ts) ->
    List.fold_left (fun t1 -> function
      | ("=", t2) -> Bin(t1, "==", t2)
      | (op, t2) -> Bin(t1, op, t2)
    ) t ts
  )
)
and add i = i |> (
  term <~> rep( (str "+." <|> str "-." <|> str "+"<|> str "-") <~> term)
  >>> (fun (t,ts) ->
    List.fold_left (fun t1 -> function
      | ("+.", t2) -> Bin(t1, "+", t2)
      | ("-.", t2) -> Bin(t1, "-", t2)
      | (op, t2) -> Bin(t1, op, t2)
    ) t ts
  )
)
and term i = i |> (
  sub <~> rep((str "*."<|>str "/."<|>str "*"<|>str "/") <~> sub )
  >>> (fun (t,ts) ->
    List.fold_left (fun t1 -> function
      | ("*.", t2) -> Bin(t1, "*", t2)
      | ("/.", t2) -> Bin(t1, "/", t2)
      | (op, t2) -> Bin(t1, op, t2)
    ) t ts
  )
)
and sub i = i |> (
  ((str "-." >> app) >>> (fun a -> Pre("-", a) )) <|>
  ((str "-" >> app) >>> (function
    | Float(f) -> Float (-. f)
    | a -> Pre("-", a)
  )) <|>
  ((str "!" >> app) >>> (fun a -> Get(a, Str "ref") )) <|>
  app
)
and app i = i |> (
  (dot <~> rep1(dot) >>> (function
    | (Var "Array.create",[a;b]) -> Array(a, b)
    | (a,b) -> App(a, b)
  )) <|>
  (cident <~> exp >>> (fun (a,b) -> CApp(a, b) )) <|>
  (cident >>> (fun a -> CApp(a, Unit) )) <|>
  dot
)
and dot i = i |> (
  (simple_exp <~> rep1(str "." >> str "(" >> exp << str ")") <~> opt(str "<-" >> _let) >>> (
    function
    | ((a,b), Some(c)) ->
      begin match List.fold_left (fun a b -> Get(a, b)) a b with
        | Get(a, b) -> Put(a, b, c)
        | _ -> assert false
      end
    | ((a,b), None) -> List.fold_left (fun a b -> Get(a, b) ) a b
  )) <|>
  (simple_exp <~> rep1(str "." >> ident) <~> opt(str "<-" >> _let) >>> (
    function
    | ((a, b), Some(c)) ->
      begin match List.fold_left (fun a b -> Get(a, Str b) ) a b with 
        | Get(a, b) -> Put(a, b, c)
        | _ -> assert false
      end
    | ((a, b), None) -> List.fold_left (fun a b -> Get(a, Str b) ) a b
  )) <|>
  (simple_exp <~> (str ":=" >> _let) >>> (
    fun (a,b) -> Put(a, Str "ref", b)
  )) <|>
  simple_exp
)
and start i = i |> (exp << skip)

let parse str =
  begin match start str with
    | Some(ps,rs) ->
      if rs <> "" then Bin(ps,"error",Raise rs)
      else ps
    | None -> Raise "error"
  end
