(lang dune 3.15)

(name friendli)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name friendli)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  discord
  base
  eio
  eio_main
  logs
  mirage-crypto-rng-eio
  dotenv
))

(package
 (name friendli-dev)
 (synopsis "Dev")
 (description "A longer description")
 (depends
   ppx_deriving
   merlin
 ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
