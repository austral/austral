(lang dune 2.1)
(using menhir 2.0)

(name austral)
(version 0.0.1)

(package
 (name austral)
 (synopsis "The bootstrapping compiler for Austral")
 (description "The bootstrapping compiler for Austral.")
 (homepage https://austral-lang.org/)
 (bug_reports https://github.com/austral/austral/issues)
 (depends
  (ppxlib (>= 0.25.1))
  (ppx_deriving (= 5.2.1))
  (ppx_sexp_conv (= v0.15.0))
  (sexplib (= v0.15.1))
  (ounit2 (= 2.2.7))
  (menhir (= 20220210))
  (zarith (= 1.12))
  (yojson (= 2.1.0))))

(generate_opam_files)
