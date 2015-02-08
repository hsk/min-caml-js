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
  | Raise
  | Rec of (string * e) list
  | Match of e * (e * e option * e) list
  | Unit
  | Tuple of e list
  | Get of e * e
  | Put of e * e * e
  | Array of e * e
  | Type of Id.t * (Id.t * Type.t list) list * e
  | CApp of Id.t * e
