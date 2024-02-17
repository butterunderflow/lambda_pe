open Sexplib.Conv

type ty =
  | TInt
  | TFun of ty * ty
[@@deriving sexp]

type expr =
  | EInt of int
  | EVar of string
  | ELam of string * ty * expr
  | ELet of string * expr * expr
  | EApp of expr * expr
[@@deriving sexp]

type value =
  | VInt of int
  | VFun of (value -> value)
[@@deriving sexp]

let get_int (v : value) =
  match v with
  | VInt v -> v
  | VFun _ -> failwith "neverreach"

let get_fun (v : value) =
  match v with
  | VInt _ -> failwith "neverreach"
  | VFun f -> f

type env = (string * value) list

let empty_env = []

let get x env = List.assoc x env

let push x v env = (x, v) :: env

let rec eval (e : expr) env : value =
  match e with
  | EInt v -> VInt v
  | EVar x -> get x env
  | ELam (x, _, e) -> VFun (fun x_val -> eval e (push x x_val env))
  | ELet (x, e0, e1) ->
      let x_value = eval e0 env in
      eval e1 (push x x_value env)
  | EApp (e0, e1) ->
      let f = eval e0 env in
      let arg = eval e1 env in
      f |> get_fun arg

(* inline tests *)

let%expect_test "Test: eval lambda" =
  let open Common in
  let print_value v = sexp_of_value v |> print_sexp in
  eval (EInt 0) empty_env |> print_value;
  [%expect {| (VInt 0) |}];
  eval (ELam ("x", TInt, EVar "x")) empty_env |> print_value;
  [%expect {| (VFun <fun>) |}]
