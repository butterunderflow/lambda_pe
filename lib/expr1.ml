open Sexplib.Conv
module StrSet = Set.Make (String)
module A = Ann

type constant =
  | CInt of int
  | CBool of bool

and op =
  | OAdd
  | OMinus
  | ONot
  | OAnd

and expr =
  | EConst of constant
  | EVar of string
  | ELam of string * expr
  | ELet of string * expr * expr
  | EApp of expr * expr
  | EAnn of expr * A.t
  | EOp of op * expr list
[@@deriving sexp]

let get_free_vars (e : expr) : string list =
  let vars = ref StrSet.empty in
  let rec go e env =
    match e with
    | EConst _ -> ()
    | EVar x -> if not (StrSet.mem x env) then vars := StrSet.add x !vars
    | ELam (x, e1) -> go e1 (StrSet.add x env)
    | ELet (x, e0, e1) ->
        go e0 env;
        go e1 (StrSet.add x env)
    | EApp (e0, e1) ->
        go e0 env;
        go e1 env
    | EAnn (e0, _) -> go e0 env
    | EOp (_op, es0) -> List.iter (fun e0 -> go e0 env) es0
  in
  go e StrSet.empty;
  StrSet.fold (fun x acc -> x :: acc) !vars []

type value =
  | VConst of constant
  | VFun of closure

and env = (string * value) list

and closure = env * string * expr [@@deriving sexp]

let get_int (v : value) =
  match v with
  | VConst (CInt v) -> v
  | VConst (CBool _)
  | VFun _ ->
      failwith "neverreach"

let get_bool (v : value) =
  match v with
  | VConst (CBool v) -> v
  | VConst (CInt _)
  | VFun _ ->
      failwith "neverreach"

let get_fun (v : value) =
  match v with
  | VConst _ -> failwith "neverreach"
  | VFun f -> f

let empty_env = []

let get x env = List.assoc x env

let push x v env = (x, v) :: env

let rec eval (e : expr) env : value =
  match e with
  | EConst v -> VConst v
  | EVar x -> get x env
  | ELam (x, e0) ->
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
  | EAnn (e0, _ann) -> eval e0 env
  | EOp (op, es0) -> (
      match (op, es0) with
      | OAdd, [ e0; e1 ] ->
          VConst (CInt (get_int (eval e0 env) + get_int (eval e1 env)))
      | OMinus, [ e0; e1 ] ->
          VConst (CInt (get_int (eval e0 env) - get_int (eval e1 env)))
      | ONot, [ e0 ] -> VConst (CBool (get_bool (eval e0 env)))
      | OAnd, [ e0; e1 ] ->
          VConst (CBool (get_bool (eval e0 env) && get_bool (eval e1 env)))
      | _ -> failwith "neverreach")

(* inline tests *)

let%expect_test "Test: eval lambda" =
  let open Common in
  let print_value v = sexp_of_value v |> print_sexp in
  eval (EConst (CInt 0)) empty_env |> print_value;
  [%expect {| (VConst (CInt 0)) |}];
  eval (ELam ("x", EVar "x")) empty_env |> print_value;
  [%expect {| (VFun (() x (EVar x))) |}];
  eval (EOp (OAdd, [ EConst (CInt 7); EConst (CInt 3) ])) empty_env
  |> print_value;
  [%expect {| (VConst (CInt 10)) |}]
