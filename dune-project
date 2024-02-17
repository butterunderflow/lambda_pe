(lang dune 3.7)

(name lambda_pe)

(generate_opam_files true)

(source
 (github butterunderflow/lambda_pe))

(authors "butterunderflow")

(maintainers "Maintainer Name")

(package
 (name lambda_pe)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "partial evaluation" "lambda calculus" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
