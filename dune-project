(lang dune 3.17)

(name weft)

(generate_opam_files true)

(source
 (github bikallem/weft))

(authors "Bikal Gurung <gbikal@gmail.com>")

(maintainers "Bikal Gurung <gbikal@gmail.com>")

(license "ISC AND MPL-2.0")

(package
 (name weft)
 (synopsis
  "Structured concurrency for OCaml 5 with CML events and a deterministic simulator")
 (description
  "Weft runs fibers on OCaml 5 domains. Each fiber is in a scope, and a scope waits for all of its fibers. Its events come from Concurrent ML. All operating system calls are effects, so a simulator can run the same program with a virtual clock and network.")
 (depends
  (ocaml
   (>= 5.5))
  (windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (dscheck
   (and
    :with-test
    (>= 0.6.0)))))

(package
 (name weft-tls)
 (allow_empty)
 (license MPL-2.0)
 (synopsis "TLS flows for weft, with ocaml-tls")
 (description
  "Weft_tls makes a TLS flow of weft.flow from another flow, as a server or as a client, with ocaml-tls. A read and a write can run at the same time in two fibers. A client can start TLS in the middle of a protocol, after bytes in clear text.")
 (depends
  (weft
   (= :version))
  (tls
   (>= 2.1.3))
  mirage-crypto-rng
  (windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (x509 :with-test)
  (domain-name :with-test)
  (ipaddr :with-test)
  (ptime :with-test)
  (dscheck
   (and
    :with-test
    (>= 0.6.0)))))
