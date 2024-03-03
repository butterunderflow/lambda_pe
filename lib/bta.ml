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
    in
    go e
end

include NaiveBTA
