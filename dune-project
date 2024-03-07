(lang dune 3.7)

(name lambda_pe)

(generate_opam_files true)

(source
 (github butterunderflow/lambda_pe))

(authors "butterunderflow")

(maintainers "Maintainer Name")

(using menhir 2.1)

(package
 (name lambda_pe)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends sexplib js_of_ocaml menhirLib ppx_sexp_conv ppx_jane ppx_inline_test js_of_ocaml-ppx)
 (tags
  (topics "partial evaluation" "lambda calculus" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
