(lang dune 1.11)
(using fmt 1.2)
(name ipl-ast)

(dialect
 (name imandra)
 (implementation
  (extension iml)
  (preprocess  (system "opam exec -- imandra-extract %{input-file}"))
  (format (system "ocamlformat %{input-file}")))
 (interface (extension "imli")))
