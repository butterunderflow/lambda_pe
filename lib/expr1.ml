open Sexplib.Conv
module StrSet = Set.Make (String)

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

let get_free_vars (e : expr) : string list =
  let vars = ref StrSet.empty in
  let rec go e env =
    match e with
    | EInt _ -> ()
    | EVar x -> if not (StrSet.mem x env) then vars := StrSet.add x !vars
    | ELam (x, _, e1) -> go e1 (StrSet.add x env)
    | ELet (x, e0, e1) ->
        go e0 env;
        go e1 (StrSet.add x env)
    | EApp (e0, e1) ->
        go e0 env;
        go e1 env
  in
  go e StrSet.empty;
  StrSet.fold (fun x acc -> x :: acc) !vars []

type value =
  | VInt of int
  | VFun of closure

and env = (string * value) list

and closure = env * string * expr [@@deriving sexp]

let get_int (v : value) =
  match v with
  | VInt v -> v
  | VFun _ -> failwith "neverreach"

let get_fun (v : value) =
  match v with
  | VInt _ -> failwith "neverreach"
  | VFun f -> f

let empty_env = []

let get x env = List.assoc x env

let push x v env = (x, v) :: env

let rec eval (e : expr) env : value =
  match e with
  | EInt v -> VInt v
  | EVar x -> get x env
  | ELam (x, _, e0) ->
      let vars = get_free_vars e in
      let captures = List.map (fun x -> (x, get x env)) vars in
      VFun (captures, x, e0)
  | ELet (x, e0, e1) ->
      let x_value = eval e0 env in
      eval e1 (push x x_value env)
  | EApp (e0, e1) ->
      let f = eval e0 env in
      let arg = eval e1 env in
      let captures, x, body = get_fun f in
      eval body (push x arg captures)

(* inline tests *)

let%expect_test "Test: eval lambda" =
  let open Common in
  let print_value v = sexp_of_value v |> print_sexp in
  eval (EInt 0) empty_env |> print_value;
  [%expect {| (VInt 0) |}];
  eval (ELam ("x", TInt, EVar "x")) empty_env |> print_value;
  [%expect {| (VFun (() x (EVar x))) |}]
