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
  let print_infered str =
    infer (Parsing.parse_expr1_string str) empty_env |> print_result
  in
  print_infered {|0|};
  [%expect {|
      (SConst (CInt 0))
      S |}];
  print_infered {| let x = 0 in x |};
  [%expect {|
      (SLet x (SConst (CInt 0)) (Var x))
      S |}];
  print_infered {| ((fun x -> fun y -> 0) : S -> D) |};
  [%expect
    {|
      (SLam x (DLam y (DLift (SConst (CInt 0)))))
      (Func (S D)) |}];
  print_infered {| (fun x -> fun y -> 0): D -> S -> S |};
  [%expect
    {|
      (SLam x (SLam y (SConst (CInt 0))))
      (Func (D (Func (S S)))) |}];
  print_infered {| 7 + 3 |};
  [%expect
    {|
      (SOp OAdd ((SConst (CInt 7)) (SConst (CInt 3))))
      S |}];
  print_infered {| (7 : D) + 3 |};
  [%expect
    {|
      (DOp OAdd ((DLift (SConst (CInt 7))) (DLift (SConst (CInt 3)))))
      D |}];

  print_infered {| (fun y -> y) : D -> D |};
  [%expect {|
    (SLam y (Var y))
    (Func (D D))
    |}];

  (* lift(delay) function with dynamic argument to dynamic(next stage) *)
  print_infered {| ((fun y -> y) : D -> D) : D |};
  [%expect {|
    (DLift (SLam y (Var y)))
    D
    |}];
  print_infered
    {|
                 ((fun x -> fun y -> 0): D -> D -> S) : D
                 |};
  [%expect {|
    (DLift (SLam x (SLam y (SConst (CInt 0)))))
    D
    |}];
  print_infered
    {|
     ((fun x -> fun y -> x): D -> D -> D) : D
     |};
  [%expect {|
    (DLift (SLam x (SLam y (Var x))))
    D
    |}]
