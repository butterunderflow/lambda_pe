let string_of_sexp = Sexplib.Sexp.to_string_hum ?indent:(Some 2)

let print_sexp s =
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s |> print_string
