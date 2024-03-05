type t =
  | S
  | D
  | Func of (t * t)
[@@deriving sexp]
