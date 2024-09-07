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

module NaiveBTA' : BTA_Sig = struct
  (* blindly stage every thing to compile time *)
  let analysis (e : E1.expr) : E2.expr =
    let rec go e =
      match e with
      | E1.EConst c -> E2.SConst c
      | E1.EVar x -> E2.Var x
      | E1.ELam (x, e0) -> E2.SLam (x, go e0)
      | E1.ELet (x, e0, e1) -> E2.SLet (x, go e0, go e1)
      | E1.EApp (e0, e1) -> E2.SApp (go e0, go e1)
      | E1.EAnn (e0, _) -> go e0
      | E1.EOp (op, es) -> E2.SOp (op, List.map go es)
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
        | OAnd, [ e0; e1 ] -> (
            let e0', a0' = infer e0 env in
            let e1' = check e1 a0' env in
            match a0' with
            | S -> (E2.SOp (op, [ e0'; e1' ]), S)
            | D -> (E2.DOp (op, [ e0'; e1' ]), D)
            | Func _ -> failwith "ill-form")
        | ONot, [ e0 ] -> (
            let e0', a0' = infer e0 env in
            match a0' with
            | S -> (E2.SOp (op, [ e0' ]), S)
            | D -> (E2.DOp (op, [ e0' ]), D)
            | Func _ -> failwith "ill-form")
        | _ -> failwith "neverreach")

  and check (e : E1.expr) (expect_ann : ann) (env : ann_env) : E2.expr =
    match e with
    | E1.ELam (x, e0) -> check_lambda x e0 expect_ann env
    | _ -> (
        let e', actual_ann = infer e env in
        if actual_ann = expect_ann then e'
        else
          match actual_ann with
          (* Any expression, which is static or function accept dynamic(in
             next stage) value, can be lifted to dynamic(next stage) value *)
          | S
          | Func (D, _)
            when expect_ann = D ->
              E2.DLift e'
          | _ -> failwith "error")

  and check_lambda x e a env =
    match a with
    | S -> failwith "error lambda annotation"
    | D ->
        let e' = check e D ((x, D) :: env) in
        E2.DLam (x, e')
    | Func (arg_ann, ret_ann) ->
        (* Expression with binding time annotation like _ -> _ (S -> D, D ->
           D, etc.) are all just staging time function. *)
        let e' = check e ret_ann ((x, arg_ann) :: env) in
        E2.SLam (x, e')

  let analysis (e : E1.expr) : E2.expr = infer e empty_env |> fst
end

include NaiveBTA
