module E1 = Expr1
module E2 = Expr2

module type BTA_Sig = sig
  val analysis : Expr1.expr -> Expr2.expr
end

module NaiveBTA : BTA_Sig = struct
  (* blindly push every thing to runtime *)
  let analysis (e : E1.expr) : E2.expr =
    let rec go e =
      match e with
      | E1.EConst c -> E2.SConst c
      | E1.EVar x -> E2.Var x
      | E1.ELam (x, e0) -> E2.DLam (x, go e0)
      | E1.ELet (x, e0, e1) -> E2.DLet (x, go e0, go e1)
      | E1.EApp (e0, e1) -> E2.DApp (go e0, go e1)
      | E1.EAnn (e0, _) -> go e0
      | E1.EOp (op, es) -> E2.DOp (op, List.map go es)
    in
    go e
end

module InferBTA = struct
  [@@@warning "-27"]

  type ann = Ann.t

  type ann_env = (string * ann) list

  let empty_env = []

  let get x env = List.assoc x env

  let rec infer (e : E1.expr) (env : ann_env) : E2.expr * ann =
    match e with
    | E1.EConst c -> (E2.SConst c, S)
    | E1.EVar x -> (E2.Var x, get x env)
    | E1.ELam (x, e0) -> failwith "can't infer a lambda"
    | E1.ELet (x, e0, e1) -> (
        let e0', a0' = infer e0 env in
        match a0' with
        | D -> (E2.DLet (x, e0', check e1 Ann.D ((x, a0') :: env)), D)
        | _ ->
            let e1', a1' = infer e1 ((x, a0') :: env) in
            (E2.SLet (x, e0', e1'), a1'))
    | E1.EApp (e0, e1) -> (
        let e0', a0' = infer e0 env in
        match a0' with
        | S -> failwith "error"
        | D ->
            let e1' = check e1 Ann.D env in
            (E2.DApp (e0', e1'), Ann.D)
        | Func (arg_ann, ret_ann) ->
            let e1' = check e1 arg_ann env in
            (E2.SApp (e0', e1'), ret_ann))
    | E1.EAnn (e0, a0) -> (check e0 a0 env, a0)
    | E1.EOp (op, es) -> (
        match (op, es) with
        | OAdd, [ e0; e1 ]
        | OMinus, [ e0; e1 ]
        | OAnd, [ e0; e1 ] ->
            let e0', a0' = infer e0 env in
            let e1' = check e1 a0' env in
            (E2.SOp (op, [ e0'; e1' ]), a0')
        | ONot, [ e0 ] ->
            let e0', a0' = infer e0 env in
            (E2.SOp (op, [ e0' ]), D)
        | _ -> failwith "neverreach")

  and check (e : E1.expr) (a : ann) (env : ann_env) : E2.expr =
    match e with
    | E1.EConst c -> check_const c a
    | E1.EVar x -> if a = get x env then E2.Var x else failwith "error"
    | E1.ELam (x, e0) -> check_lambda x e0 a env
    | _ ->
        let e', a' = infer e env in
        if a' = a then e' else failwith "error"

  and check_const c a =
    match a with
    | S -> E2.SConst c
    | D -> E2.DLift (E2.SConst c)
    | Func _ -> failwith "error const"

  and check_lambda x e a env =
    match a with
    | S -> failwith "error lambda annotation"
    | D ->
        let e' = check e D ((x, D) :: env) in
        E2.DLam (x, e')
    | Func (arg_ann, ret_ann) ->
        let e' = check e ret_ann ((x, arg_ann) :: env) in
        E2.SLam (x, e')

  let analysis (e : E1.expr) : E2.expr = infer e empty_env |> fst

  let%expect_test "Test: inference binding time annotation" =
    let open Common in
    let print_result (e, a) =
      E2.sexp_of_expr e |> print_sexp;
      print_newline ();
      Ann.sexp_of_t a |> print_sexp;
      print_newline ()
    in
    infer (EConst (CInt 0)) empty_env |> print_result;
    [%expect {|
      (SConst (CInt 0))
      S |}];
    infer (ELet ("x", EConst (CInt 0), EVar "x")) empty_env |> print_result;
    [%expect {|
      (SLet x (SConst (CInt 0)) (Var x))
      S |}];
    infer
      (EAnn (ELam ("x", ELam ("y", EConst (CInt 0))), Func (S, D)))
      empty_env
    |> print_result;
    [%expect
      {|
      (SLam x (DLam y (DLift (SConst (CInt 0)))))
      (Func (S D)) |}];
    infer
      (EAnn (ELam ("x", ELam ("y", EConst (CInt 0))), Func (D, Func (S, S))))
      empty_env
    |> print_result;
    [%expect
      {|
      (SLam x (SLam y (SConst (CInt 0))))
      (Func (D (Func (S S)))) |}];
    infer (EOp (OAdd, [ EConst (CInt 7); EConst (CInt 3) ])) empty_env
    |> print_result;
    [%expect
      {|
      (SOp OAdd ((SConst (CInt 7)) (SConst (CInt 3))))
      S |}];
    infer
      (EOp (OAdd, [ EAnn (EConst (CInt 7), D); EConst (CInt 3) ]))
      empty_env
    |> print_result;
    [%expect
      {|
      (SOp OAdd ((DLift (SConst (CInt 7))) (DLift (SConst (CInt 3)))))
      D |}]
end

include NaiveBTA
