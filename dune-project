(lang dune 3.13)
(name gemini)
(version 0.1.0)
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
   lwt
   cohttp-lwt-unix
   yojson
   uri
 )
)
(package
 (name gemini-cli)
 (synopsis "Command line interface to Gemini")
 (depends
   gemini
 )
)
