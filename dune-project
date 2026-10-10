(lang dune 3.17)

(name weft)

(generate_opam_files true)

(source
 (github bikallem/weft))

(authors "Bikal Gurung <gbikal@gmail.com>")

(maintainers "Bikal Gurung <gbikal@gmail.com>")

(license ISC)

(package
 (name weft)
 (synopsis "Concurrency for OCaml 5 with CML events and a deterministic simulator")
 (description
  "Weft runs fibers on OCaml 5 domains. Its events come from Concurrent ML. All operating system calls are effects, so a simulator can run the same program with a virtual clock and network.")
 (depends
  (ocaml
   (>= 5.3))))
