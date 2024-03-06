open Sexplib.Conv
module E1 = Expr1

type ty =
  | TInt
  | TFun of ty * ty
[@@deriving sexp]

type constant = E1.constant

and op = E1.op

and expr =
  (* use variable lookup as specialize result *)
  | Var of string
  (* specialize to a value(a pe time int or function value) *)
  | SConst of constant
  | SLam of string * expr
  | SLet of string * expr * expr
  | SApp of expr * expr
  | SOp of op * expr list
  (* specialize to a pe-time code expression *)
  | DLam of string * expr
  | DLet of string * expr * expr
  | DApp of expr * expr
  | DOp of op * expr list
  | DLift of expr
[@@deriving sexp]

type env = (string * value) list

and value =
  | VConst of constant
  | VFun of (value -> value)
  | VCode of code

and code = Expr1.expr [@@deriving sexp]

let build_lambda x e = E1.ELam (x, e)

let get_const (v : value) =
  match v with
  | VConst c -> c
  | VFun _
  | VCode _ ->
      failwith "ill-form"

let get_int (v : value) =
  match v with
  | VConst (CInt v) -> v
  | VConst (CBool _)
  | VFun _
  | VCode _ ->
      failwith "ill-form"

let get_bool (v : value) =
  match v with
  | VConst (CBool v) -> v
  | VConst (CInt _)
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

let var_index = ref 0

let gen_var ~(hint : string) : code =
  var_index := !var_index + 1;
  let name = Printf.sprintf "%s_%d" hint !var_index in
  E1.EVar name

let empty_env = []

let rec eval (e : expr) (env : env) : value =
  match e with
  | Var x -> List.assoc x env
  | DLam (x, e) ->
      let new_var = gen_var ~hint:x in
      VCode (E1.ELam (x, eval e ((x, VCode new_var) :: env) |> get_code))
  | DLet (x, e0, e1) ->
      let updated_env = (x, VCode (E1.EVar x)) :: env in
      VCode
        (match eval e0 env with
        | VCode code -> E1.ELet (x, code, eval e1 updated_env |> get_code)
        | VConst c ->
            E1.ELet (x, E1.EConst c, eval e1 updated_env |> get_code)
        | VFun f ->
            E1.ELet
              ( x,
                f (VCode (E1.EVar "_x")) |> get_code,
                eval e1 updated_env |> get_code ))
  | DApp (e0, e1) ->
      VCode (E1.EApp (eval e0 env |> get_code, eval e1 env |> get_code))
  | DOp (op, es) ->
      VCode (E1.EOp (op, List.map (fun e0 -> eval e0 env |> get_code) es))
  | DLift e ->
      let v = get_const (eval e env) in
      VCode (E1.EConst v)
  | SConst c -> VConst c
  | SLam (x, e) -> VFun (fun v -> eval e ((x, v) :: env))
  | SLet (x, e0, e1) ->
      let bind_value = eval e0 env in
      eval e1 ((x, bind_value) :: env)
  | SOp (op, es0) -> (
      match (op, es0) with
      | OAdd, [ e0; e1 ] ->
          VConst (CInt (get_int (eval e0 env) + get_int (eval e1 env)))
      | OMinus, [ e0; e1 ] ->
          VConst (CInt (get_int (eval e0 env) - get_int (eval e1 env)))
      | ONot, [ e0 ] -> VConst (CBool (get_bool (eval e0 env)))
      | OAnd, [ e0; e1 ] ->
          VConst (CBool (get_bool (eval e0 env) && get_bool (eval e1 env)))
      | _ -> failwith "neverreach")
  | SApp (e0, e1) ->
      let func = get_func (eval e0 env) in
      eval e1 env |> func

let%expect_test "Test: eval 2level lambda" =
  let open Common in
  let print_value v = sexp_of_value v |> print_sexp in
  eval (SConst (CInt 0)) empty_env |> print_value;
  [%expect {| (VConst (CInt 0)) |}];
  eval (SApp (SLam ("x", Var "x"), SConst (CInt 1))) empty_env |> print_value;
  [%expect {| (VConst (CInt 1)) |}];
  (* ((slambda x 77), y) , {y = EVar "xxxx"} *)
  eval
    (SApp (SLam ("x", SConst (CInt 77)), Var "y"))
    [ ("y", VCode (E1.EVar "xxxx")) ]
  |> print_value;
  [%expect {| (VConst (CInt 77)) |}];
  eval (SOp (OAdd, [ SConst (CInt 7); SConst (CInt 3) ])) empty_env
  |> print_value;
  [%expect {| (VConst (CInt 10)) |}];
  eval
    (DOp (OAdd, [ DLift (SConst (CInt 7)); DLift (SConst (CInt 3)) ]))
    empty_env
  |> print_value;
  [%expect {| (VCode (EOp OAdd ((EConst (CInt 7)) (EConst (CInt 3))))) |}]
