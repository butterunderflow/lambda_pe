open Lambda_pe_lib
open Common
open Expr1

let%expect_test "Test: eval lambda" =
  let print_value v = sexp_of_value v |> print_sexp in
  eval (EConst (CInt 0)) empty_env |> print_value;
  [%expect {| (VConst (CInt 0)) |}];
  eval (ELam ("x", EVar "x")) empty_env |> print_value;
  [%expect {| (VFun (() x (EVar x))) |}];
  eval (EOp (OAdd, [ EConst (CInt 7); EConst (CInt 3) ])) empty_env
  |> print_value;
  [%expect {| (VConst (CInt 10)) |}]
