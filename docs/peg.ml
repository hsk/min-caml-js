module Peg = struct
  let white_space = ref (fun i ->
    Str.replace_first (Str.regexp "[ \r\n\t]*") "" i
  )

  let change_white_space f =
    white_space := f

  let rec startsWith i param =
    let ilen = String.length i in
    let len = String.length param in
    if len > ilen then false
    else String.sub i 0 len = param

  let subn i n =
    String.sub i n (String.length i - n)

  let rec str param i =
    let i = !white_space i in
    if(startsWith i param)
    then
      let len = String.length param in
      Some(param, subn i len)
    else None

  let rec reg param i =
    let i = !white_space i in
    if Str.string_match (Str.regexp param) i 0 then
      Some((Str.matched_string i),(Str.string_after i (Str.match_end())))
    else None

  let any_char () i =
    if(String.length i > 0)
    then Some(String.sub i 0 1, subn i 1)
    else None

  let rec (<|>) this that i =
    match this i with
    | None -> that i
    | e -> e

  let rec (<~>) this that i =
    begin match this i with
    | None -> None
    | Some(r1, rest1) ->
      begin match that rest1 with
      | None -> None
      | Some(r2, rest2) -> Some((r1, r2), rest2)
      end
    end

  let rec (>>) this that i =
    match (this <~> that) i with
    | None -> None
    | Some((_,b), rs) -> Some(b, rs)

  let rec (<<) this that i =
    match (this <~> that) i with
    | None -> None
    | Some((b,_), rs) -> Some(b, rs)

  let rec (^^) this f i =
    match this i with
    | Some(r, rest) -> Some(f r, rest)
    | None -> None
  let rec (>>>) a b = a ^^ b

  let rec (>?>) this f i =
    try
      match this i with
      | Some(r, rest) -> Some(f r, rest)
      | None -> None
    with
      | _ -> None

  let rec opt this i =
    match this i with
    | Some (p1,p2) -> Some(Some(p1), p2)
    | None -> Some(None, i)

  let rec rep this i =
    let rec parse i =
      begin match this i with
      | None -> Some([], i)
      | Some((r, rest)) ->
        begin match parse rest with
        | None -> Some([r], rest)
        | Some(r2, rest2) ->  Some(r::r2, rest2)
        end
      end
    in
    parse i

  let rec rep1 this = (this <~> (rep this)) ^^ (fun (p1,p2) -> p1::p2)

  let rec notp = (fun i ->
    match i with
    | Some(_) -> None
    | None -> Some((), i)
  )
  (*let rec andp p = notp(notp p)*)
end


module Test = struct
  open Peg

  let _ =
    let parser = str "test" in
    match parser "test123" with
    | None -> Format.printf "None@."
    | Some(p,rest) -> Format.printf "Some(\"%s\",\"%s\")@." p rest

  let _ =
    let parser = str "test" <|> str "abc" in
    match parser "abc123" with
    | None -> Format.printf "None@."
    | Some(p,rest) -> Format.printf "Some(\"%s\",\"%s\")@." p rest

  let _ =
    let parser = str "abc" <~> str "123" in
    match parser "abc12345" with
    | None -> Format.printf "None@."
    | Some((p1,p2),rest) -> Format.printf "Some(\"%s\",\"%s\",\"%s\")@." p1 p2 rest

  let _ =
    let parser = str "123" ^^ (fun n -> int_of_string n) in
    match parser "12345" with
    | None -> Format.printf "None@."
    | Some(p,rest) -> Format.printf "Some(%d,\"%s\")@." p rest

  let _ =
    let parser = opt (str "123") in
    begin match parser "abc12345" with
    | None -> Format.printf "None@."
    | Some(None,i) -> Format.printf "Some(None,\"%s\")@." i
    | Some(Some p,i) -> Format.printf "Some(Some(\"%s\"),\"%s\")@." p i
    end;
    begin match parser "123456" with
    | None -> Format.printf "None@."
    | Some(None,i) -> Format.printf "Some(None,\"%s\")@." i
    | Some(Some p,i) -> Format.printf "Some(Some(\"%s\"),\"%s\")@." p i
    end

  let _ =
    let digit =
      str "0" <|>
      str "1" <|>
      str "2" <|>
      str "3" <|>
      str "4" <|>
      str "5" <|>
      str "6" <|>
      str "7" <|>
      str "8" <|>
      str "9"
     in
    let rec p_s f = function
      | [] -> ()
      | x::xs -> Format.fprintf f "%s;%a" x p_s xs
    in 
    let parser = rep digit in
    begin match parser "12345 a b" with
      | None -> Format.printf "None@."
      | Some(ps,rest) -> Format.printf "Some([%a],\"%s\")@." p_s ps rest
    end

  let _ =
    let digit = reg "[1-9][0-9]+" in
    begin match digit " 12345 a b" with
      | None -> Format.printf "None@."
      | Some(ps,rest) -> Format.printf "Some(\"%s\",\"%s\")@." ps rest
    end
end

module Test = struct
  open Peg

  let rec exp i = i |> (
    term <~> rep( (str "+"<|> str "-") <~> term) >>> (fun (t,ts) ->
      List.fold_left (fun t1 (op,t2) -> "(" ^ t1 ^ " " ^ op ^ " " ^ t2 ^ ")") t ts
    )
  )
  and term i = i |> (
    fact <~> rep((str "*"<|>str "/") <~> fact ) >>> (fun (t,ts) ->
      List.fold_left (fun t1 (op,t2) -> "(" ^ t1 ^ " " ^ op ^ " " ^ t2 ^ ")") t ts
    )
  )
  and fact i = i |> (
    reg "[1-9][0-9]*" <|>
    (str "(" >> exp << str ")") ^^ (fun e -> e)
  )

  let parse str =
  begin match exp str with
    | Some(ps,rs) ->
      let rs = !white_space rs in
      if rs <> "" then Format.sprintf"(%s,%s)@?" ps rs
      else ps
    | None -> "error"
  end

  let _ =
    Format.printf "%s@." (parse "1*2+2/3")
end

module Test = struct

  module Parser = struct
    open Peg

    let rec exp i = i |> (
      term <~> rep( (str "+"<|> str "-") <~> term) >>> (fun (t,ts) ->
        List.fold_left (fun t1 (op,t2) -> "(" ^ t1 ^ " " ^ op ^ " " ^ t2 ^ ")") t ts
      )
    )
    and term i = i |> (
      fact <~> rep((str "*"<|>str "/") <~> fact ) >>> (fun (t,ts) ->
        List.fold_left (fun t1 (op,t2) -> "(" ^ t1 ^ " " ^ op ^ " " ^ t2 ^ ")") t ts
      )
    )
    and fact i = i |> (
      reg "[1-9][0-9]*" <|>
      (str "(" >> exp << str ")") ^^ (fun e -> e)
    )

    let parse str =
    begin match exp str with
      | Some(ps,rs) ->
        let rs = !white_space rs in
        if rs <> "" then Format.sprintf"(%s,%s)@?" ps rs
        else ps
      | None -> "error"
    end
  end

  let _ =
    Format.printf "%s@." (Parser.parse "1*2+2/3")
end


module Test = struct

  type e =
  | Int of int
  | Bin of e * string * e
  | Raise of string

  let rec show_e f = function
  | Int i -> Format.fprintf f "Int(%d)@?" i
  | Bin(e1,op,e2) -> Format.fprintf f "Bin(%a,%s,%a)@?" show_e e1 op show_e e2
  | Raise s -> Format.fprintf f "Error(%s)@?" s

  module Parser = struct
    open Peg

    let rec exp i = i |> (
      term <~> rep( (str "+"<|> str "-") <~> term) >>> (fun (t,ts) ->
        List.fold_left (fun t1 (op,t2) -> Bin(t1, op, t2)) t ts
      )
    )
    and term i = i |> (
      fact <~> rep((str "*"<|>str "/") <~> fact ) >>> (fun (t,ts) ->
        List.fold_left (fun t1 (op,t2) -> Bin(t1, op, t2)) t ts
      )
    )
    and fact i = i |> (
      (reg "[1-9][0-9]*" >>> (fun e -> Int(int_of_string e))) <|>
      (str "(" >> exp << str ")") >>> (fun e -> e)
    )

    let parse str =
    begin match exp str with
      | Some(ps,rs) ->
        let rs = !white_space rs in
        if rs <> "" then Bin(ps,"error",Raise rs)
        else ps
      | None -> Raise "error"
    end
  end

  let _ =
    Format.printf "%a@." show_e (Parser.parse "1*2+2/3")
end

module Test = struct

  type e =
    | Int of int
    | Float of float
    | Bool of bool
    | Str of string
    | Var of string
    | Pre of string * e
    | Bin of e * string * e
    | If of e * e * e
    | Fun of string list * e
    | App of e * e list
    | Let of string * e * e
    | LetRec of string * e * e
    | Raise of string
    | Rec of (string * e) list
    | Match of e * (e * e option * e) list
    | Unit
    | Tuple of e list
    | Get of e * e
    | Put of e * e * e
    | Array of e * e
    | CApp of string * e

  let rec show_e f = function
  | Int i -> Format.fprintf f "Int(%d)@?" i
  | Var i -> Format.fprintf f "Var(\"%s\")@?" i
  | Str i -> Format.fprintf f "Str(\"%s\")@?" i
  | Fun(ss,e) -> Format.fprintf f "Fun([%s],%a)@?" (String.concat ";" ss) show_e e
  | App(e,es) -> Format.fprintf f "App(%a,[%a])@?" show_e e show_es es
  | Rec(ses) -> Format.fprintf f "Rec([%a])@?" show_ses ses
  | Float i -> Format.fprintf f "Float(%f)@?" i
  | Bool i -> Format.fprintf f "Bool(%b)@?" i
  | Bin(e1,op,e2) -> Format.fprintf f "Bin(%a,%s,%a)@?" show_e e1 op show_e e2
  | If(e1,e2,e3) -> Format.fprintf f "If(%a,%a,%a)@?" show_e e1 show_e e2 show_e e3
  | Let(s,e1,e2) -> Format.fprintf f "Let(\"%s\",%a,%a)@?" s show_e e1 show_e e2
  | LetRec(s,e1,e2) -> Format.fprintf f "LetRec(\"%s\",%a,%a)@?" s show_e e1 show_e e2
  | Pre(op,e2) -> Format.fprintf f "Pre(%s,%a)@?" op show_e e2
  | Raise s -> Format.fprintf f "Raise(%s)@?" s
  | Unit -> Format.fprintf f "Unit@?"
  | Tuple es -> Format.fprintf f "Tuple[%a]@?" show_es es
  | _ -> Format.fprintf f "[tes]"
  and show_es f = function
  | [] -> ()
  | [x] -> Format.fprintf f "%a" show_e x
  | x::xs -> Format.fprintf f "%a;%a" show_e x show_es xs
  and show_ses f = function
  | [] -> ()
  | [(s,x)] -> Format.fprintf f "(\"%s\",%a)" s show_e x
  | (s,x)::xs -> Format.fprintf f "(\"%s\",%a);%a" s show_e x show_ses xs

  module Parser = struct
    open Peg

    let _ =
      change_white_space (fun i ->
        Str.replace_first (Str.regexp "\\([ \r\n\t]*\\|(\\*.*\\*)\\)+") "" i
      )

    let keywords = [
      "let"; "in"; "if"; "else"; "then"; "true"; "false";
      "match"; "with"; "when"; "begin"; "end"; "type"; "as"
    ]

    let ident =
      reg "[_a-z][_a-zA-Z0-9]*\\|\\([_A-Z][_a-zA-Z0-9]*\\.\\)+[_a-z][_a-zA-Z0-9]*" >?> (function
        | "_" -> genid ""
        | a when not(List.mem a keywords) -> a
        | _ -> failwith "error"
      )

    let cident =
      reg "[A-Z][_a-zA-Z0-9]*" >?> (function
        | a when not(List.mem a keywords) -> a
        | _ -> failwith "error"
      )

    let rec simple_exp i = i |> (
      (str "true" >>> (fun _ -> Bool true)) <|>
      (str "false" >>> (fun _ -> Bool false)) <|>
      (reg "-?\\([1-9][0-9]*\\.\\|0\\.\\)[0-9]*" >>> (fun a -> Float(float_of_string a))) <|>
      (reg "-?\\([1-9][0-9]*\\|0\\)" >>> (fun e -> Int(int_of_string e))) <|>
      (str "(" >> str ")" >>> (fun _ -> Unit)) <|>
      (str "[" >> str "]" >>> (fun _ -> CApp("Nil", Unit) )) <|>
      ((str "[" >> _let) <~> rep(str ";" >> _let) << str "]" >>> (fun (a,bs) ->
        List.fold_right (fun b a ->
          CApp("Cons", Tuple([a; b]))
        ) (a :: bs) (CApp("Nil", Unit))
      )) <|>
      (ident >>> (fun a -> Var a)) <|>
      (cident <~> rep1(str "." >> ident) >>> (fun (a,b) -> Var (String.concat "." (a::b) ))) <|>
      (str "begin" >> exp << str "end" >>> (fun e -> e)) <|>
      (str "(" >> exp << str ")" >>> (fun e -> e))
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
      (str "type" >> ident >> str "=" >> opt(str "|") >> consts >> str ";;" >> exp) <|>
      tuple
    )
    and types i = i |> (
      ident <~> rep(str "*" >> ident) >>> (fun (a, b) -> a :: b)
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
      ((str "-" >> app) >>> (fun a -> Pre("-", a) )) <|>
      app
    )
    and app i = i |> (
      (dot <~> rep1(dot) >>> (fun (a,b) -> App(a, b) )) <|>
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
      simple_exp
    )

    let parse str =
    begin match exp str with
      | Some(ps,rs) ->
        let rs = !white_space rs in
        if rs <> "" then Bin(ps,"error",Raise rs)
        else ps
      | None -> Raise "error"
    end
  end

  let _ =
    Format.printf "%a@." show_e (Parser.parse "1*2+ -2/3.0+true+()+begin abc_+_+Abc . d end")

  let _ =
    Format.printf "%a@." show_e (Parser.parse "-b +. -.a")
end
