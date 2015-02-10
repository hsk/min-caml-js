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
  | Unit
  | Get of e * e
  | Put of e * e * e
  | Array of e * e
  | CApp of string * e
  | Tuple of e list
  | Match of e * (e * e option * e) list

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "T%s_%d" s !counter

let gentmp () =
  incr counter;
  Printf.sprintf "T%d" !counter

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
  | Array(e1,e2) -> Format.fprintf f "Array(%a,%a)" show_e e1 show_e e2
  | Get(e1,e2) ->
    Format.fprintf f "Get(%a,%a)@?" show_e e1 show_e e2
  | Put(e1,e2, e3) ->
    Format.fprintf f "Put(%a,%a,%a)@?" show_e e1 show_e e2 show_e e3
  | Match(e1,eees) ->
    Format.fprintf f "Match(%a,[%a])@?" show_e e1 show_eoees eees
  | CApp(s,e) -> Format.fprintf f "CApp(%s,%a)@?" s show_e e

and show_es f = function
  | [] -> ()
  | [x] -> Format.fprintf f "%a" show_e x
  | x::xs -> Format.fprintf f "%a;%a" show_e x show_es xs

and show_ses f = function
  | [] -> ()
  | [(s,x)] -> Format.fprintf f "(\"%s\",%a)" s show_e x
  | (s,x)::xs -> Format.fprintf f "(\"%s\",%a);%a" s show_e x show_ses xs

and show_eoees f = function
  | [] -> ()
  | [x] -> Format.fprintf f "%a" show_eoee x
  | x::xs -> Format.fprintf f "%a;%a" show_eoee x show_eoees xs

and show_eoee f = function
  | (e1,Some e2,e3) -> Format.fprintf f "(%a,Some %a,%a)" show_e e1 show_e e2 show_e e3
  | (e1,None, e3) -> Format.fprintf f "(%a,None,%a)" show_e e1 show_e e3
