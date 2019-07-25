(lang dune 1.10)

(name odoc2docset)
(source (github jfeser/odoc2docset))
(license MIT)
(authors "Jack Feser <feser@csail.mit.edu>")
(maintainers "Jack Feser <feser@csail.mit.edu>")

(package
  (name odoc2docset)
  (synopsis "Generates Dash docsets using Odoc")
  (description "Generates Dash docsets for OCaml packages using the Odoc documentation generator.")
  (depends
    (dune (and :build (>= 1.10)))

    (logs (>= 0.6))
    (base (>= v0.12))
    (stdio (>= v0.12))
    (odig (>= 0.0.4))
    (odoc (>= 1.4))
    (sqlite3 (>= 4.4))
    (lambdasoup (>= 0.6))
    (cmdliner (>= 1.0))
    (fmt (>= 0.8))
    ))

(using fmt 1.1)
(using menhir 2.0)
(generate_opam_files true)

; Local Variables:
; mode: dune
; End:
