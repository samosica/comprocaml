(lang dune 3.7)

(name comprocaml)

(generate_opam_files true)

(source
 (github samosica/comprocaml))

(authors "samosica <samosic4@gmail.com>")

(maintainers "samosica <samosic4@gmail.com>")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name comprocaml)
 (synopsis "An OCaml library for competitive programming")
 (depends ocaml dune)
 (tags
  ("competitive programming" "algorithms" "data structures")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
