open Lambda_pe_lib
open Common
open Expr2

let%expect_test "Test: eval 2level lambda" =
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
