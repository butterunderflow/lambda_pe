open Js_of_ocaml
open Lambda_pe_lib

let handle program =
  Printf.printf "%s\n" program;
  flush stdout

let eval_expr1 program =
  let e = Parsing.parse_expr1_string program in
  Expr1.(eval e empty_env |> sexp_of_value |> Common.string_of_sexp)

let eval_expr2 program =
  let e = Parsing.parse_expr2_string program in
  Expr2.(eval e empty_env |> sexp_of_value |> Common.string_of_sexp)

let bta program =
  let e = Parsing.parse_expr1_string program in
  Bta.InferBTA.analysis e |> Expr2.sexp_of_expr |> Common.string_of_sexp

let main () =
  Js.export "programHandler"
    (object%js
       method naiveHandle = handle

       method evalExpr1 = eval_expr1

       method evalExpr2 = eval_expr2

       method bta = bta
    end)

let () = main ()
