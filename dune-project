(lang dune 3.21)

(name olint)

(generate_opam_files true)

(source
 (github Zaneham/Olint))

(license Apache-2.0)

(package
 (name olint)
 (synopsis "A linter for OCaml. The first serious one.")
 (depends
  (ocaml
   (>= 5.4.0))
  (cmdliner
   (>= 1.3.0))
  (dune
   (>= 3.21))))
