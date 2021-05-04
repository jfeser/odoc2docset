(lang dune 2.0)

(name odoc2docset)

(source
 (github jfeser/odoc2docset))

(license MIT)

(authors "Jack Feser <feser@csail.mit.edu>")

(maintainers "Jack Feser <feser@csail.mit.edu>")

(documentation https://jfeser.github.io/odoc2docset/)

(package
 (name odoc2docset)
 (synopsis "Generates Dash docsets using Odoc")
 (description
   "Generates Dash docsets for OCaml packages using the Odoc documentation generator.")
 (depends
  (dune
   (and
    :build
    (>= 1.10)))
  (logs
   (>= 0.6))
  (core
   (>= v0.14))
  (ppx_let
   (>= v0.12))
  (odoc
   (>= 2))
  (sqlite3
   (>= 4.4))
  (lambdasoup
   (>= 0.6))
  (cmdliner
   (>= 1.0))
  (fmt
   (>= 0.8))
  (shell
   (>= v0.14))))

(using menhir 2.0)

(generate_opam_files true)

; Local Variables:
; mode: dune
; End:
