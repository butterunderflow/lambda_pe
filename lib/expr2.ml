open Sexplib.Conv

type ty =
  | TInt
  | TFun of ty * ty
[@@deriving sexp]

type expr =
  (* specialize to a value(a pe time int or function value) *)
  | SConst of constant
  | SLam of string * expr
  | SLet of string * expr * expr
  | SApp of expr * expr
  (* specialize to a pe-time code expression *)
  | DVar of string
  | DLam of string * expr
  | DLet of string * expr * expr
  | DApp of expr * expr
  | DLift of expr

and constant =
  | CInt of int
  | CBool of bool
[@@deriving sexp]

type env = (string * value) list

and value =
  | VConst of constant
  | VFun of (value -> value)
  | VCode of code

and code =
  | E1Const of constant
  | E1Var of string
  | E1Lam of string * code
  | E1Let of string * code * code
  | E1App of code * code
[@@deriving sexp]

let build_lambda x e = E1Lam (x, e)

let get_int (v : value) =
  match v with
  | VConst c -> c
  | VFun _
  | VCode _ ->
      failwith "ill-form"

let get_func (v : value) =
  match v with
  | VFun f -> f
  | VConst _
  | VCode _ ->
      failwith "ill-form"

let get_code (v : value) =
  match v with
  | VCode c -> c
  | VConst _
  | VFun _ ->
      failwith "ill-form"

let empty_env = []

let rec eval (e : expr) (env : env) : value =
  match e with
  | DVar x -> List.assoc x env
  | DLam (x, e) ->
      VCode (E1Lam (x, eval e ((x, VCode (E1Var x)) :: env) |> get_code))
  | DLet (x, e0, e1) ->
      let updated_env = (x, VCode (E1Var x)) :: env in
      VCode
        (match eval e0 env with
        | VCode code -> E1Let (x, code, eval e1 updated_env |> get_code)
        | VConst c -> E1Let (x, E1Const c, eval e1 updated_env |> get_code)
        | VFun f ->
            E1Let
              ( x,
                f (VCode (E1Var "_x")) |> get_code,
                eval e1 updated_env |> get_code ))
  | DApp (e0, e1) ->
      VCode (E1App (eval e0 env |> get_code, eval e1 env |> get_code))
  | DLift e ->
      let v = get_int (eval e env) in
      VCode (E1Const v)
  | SConst c -> VConst c
  | SLam (x, e) -> VFun (fun v -> eval e ((x, v) :: env))
  | SLet (x, e0, e1) ->
      let bind_value = eval e0 env in
      eval e1 ((x, bind_value) :: env)
  | SApp (e0, e1) ->
      let func = get_func (eval e0 env) in
      eval e1 env |> func

let%expect_test "Test: eval 2level lambda" =
  let open Common in
  let print_value v = sexp_of_value v |> print_sexp in
  eval (SConst (CInt 0)) empty_env |> print_value;
  [%expect {| (VConst (CInt 0)) |}];
  eval (SApp (SLam ("x", DVar "x"), SConst (CInt 1))) empty_env
  |> print_value;
  [%expect {| (VConst (CInt 1)) |}];
  (* ((slambda x 77), y) , {y = EVar "xxxx"} *)
  eval
    (SApp (SLam ("x", SConst (CInt 77)), DVar "y"))
    [ ("y", VCode (E1Var "xxxx")) ]
  |> print_value;
  [%expect {| (VConst (CInt 77)) |}]
