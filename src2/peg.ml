let rec startsWith i param =
  let ilen = String.length i in
  let len = String.length param in
  if len > ilen then false
  else String.sub i 0 len = param

let subn i n =
  String.sub i n (String.length i - n)

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

let rec notp this i =
  match this i with
  | Some(_,_) -> None
  | None -> Some("", i)

let rec nstr param i =
  if(startsWith i param)
  then
    let len = String.length param in
    Some(param, subn i len)
  else None

let range c1 c2 i =
  if(String.length i <= 0)
  then None
  else
    let c = String.get i 0 in
    if c1 <= c && c <= c2 then
      Some(String.sub i 0 1, subn i 1)
    else None

let rec skip i = i |> (
  rep(
    nstr " " <|>
    nstr "\r" <|>
    nstr "\n" <|>
    nstr "\t" <|>
    (nstr "(*" >> rep( notp (nstr "*)") >> any_char ()) >> nstr "*)")
  ) >>> (fun a -> "")
)
let rec str param i = i |> (skip >> nstr param)

(*
let rec nreg param i =
  if Str.string_match (Str.regexp param) i 0 then
    Some((Str.matched_string i),(Str.string_after i (Str.match_end())))
  else None

let rec reg param i = i |> (skip >> nreg param )
*)

