let parse_expr1_string string =
  let lexbuf = Lexing.from_string string in
  let out = Parser.program1 Lexer.token lexbuf in
  out

let parse_expr2_string string =
  let lexbuf = Lexing.from_string string in
  let out = Parser.program2 Lexer.token lexbuf in
  out

let%expect_test "Test: parsing expr1" =
  let open Common in
  let test s = parse_expr1_string s |> Expr1.sexp_of_expr |> print_sexp in
  test {| fun x -> x |};
  [%expect {| (ELam x (EVar x)) |}];
  test {| 1 + 3 + 6 |};
  [%expect
    {|
    (EOp OAdd
      ((EOp OAdd ((EConst (CInt 1)) (EConst (CInt 3)))) (EConst (CInt 6)))) |}];
  test {| ((fun x -> x) 1 + 1): S |};
  [%expect
    {|
    (EAnn
      (EApp (ELam x (EVar x)) (EOp OAdd ((EConst (CInt 1)) (EConst (CInt 1)))))
      S) |}]

let%expect_test "Test: parsing expr2" =
  let open Common in
  let test s = parse_expr2_string s |> Expr2.sexp_of_expr |> print_sexp in
  test {| fun x -> x |};
  [%expect {| (DLam x (Var x)) |}];
  test {| 1 s+ 1 |};
  [%expect {| (SOp OAdd ((SConst (CInt 1)) (SConst (CInt 1)))) |}];
  test {| sfun x -> x + 1 lift |};
  [%expect {| (SLam x (DOp OAdd ((Var x) (DLift (SConst (CInt 1)))))) |}]
