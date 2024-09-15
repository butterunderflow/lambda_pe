open Lambda_pe_lib
open Common
open Expr2

let print_value v = sexp_of_value v |> print_sexp

let%expect_test "Test: eval 2level lambda" =
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

let%expect_test "Test: full partial evaluation" =
  let print_partial_evaled str =
    reset_index ();
    let annotated, _annotation =
      Bta.InferBTA.infer (Parsing.parse_expr1_string str) empty_env
    in
    eval annotated empty_env |> print_value
  in
  print_partial_evaled {| ((fun y -> y) : D -> D) : D |};
  [%expect {| (VCode (ELam x_1 (EVar x_1))) |}];
  print_partial_evaled
    {|
     ((fun x -> fun y -> 0): D -> D -> S) : D
     |};
  [%expect {| (VCode (ELam x_1 (ELam x_2 (EConst (CInt 0))))) |}];
  print_partial_evaled
    {|
     ((fun x -> fun y -> x) : D -> D -> D) : D
     |};
  [%expect {| (VCode (ELam x_1 (ELam x_2 (EVar x_1)))) |}]
