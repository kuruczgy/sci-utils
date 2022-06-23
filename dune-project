(lang dune 2.9)

(name sci-utils)

(authors "kuruczgy")

(maintainers "kuruczgy")

(generate_opam_files true)

(package
 (name sci-utils)
 (synopsis "Utilities for scientific calculations")
 (depends
  (ocaml
   (>= 4.10.2))
  (dune
   (>= 2.0))
  (base :build)
  (re :build)
  (ppx_jane :build)
  (ppx_deriving :build)
  (ocamlformat :dev)
  (ocamlformat-rpc :dev)
  (ocaml-lsp-server :dev)))
