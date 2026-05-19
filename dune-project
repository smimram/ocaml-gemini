(lang dune 3.13)
(name gemini)
(generate_opam_files true)
(source (github smimram/ocaml-gemini))
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(license MIT)
(package
 (name gemini)
 (synopsis "OCaml library for interacting with the Gemini API")
 (description "OCaml bindings and client utilities for the Gemini API.")
 (depends
   ocaml
   dune
   lwt
   cohttp-lwt-unix
   yojson
   uri
 )
)
