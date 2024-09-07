open Lambda_pe_lib
open Common
open Bta.InferBTA
module E2 = Expr2

let%expect_test "Test: inference binding time annotation" =
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
      (DOp OAdd ((DLift (SConst (CInt 7))) (DLift (SConst (CInt 3)))))
      D |}]
