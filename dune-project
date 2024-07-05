(lang dune 3.0)
(formatting (enabled_for ocaml))
(using menhir 2.0)

(cram enable)
(implicit_transitive_deps false)
(generate_opam_files true)

(name "xapi")
(source (github xapi-project/xen-api))
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(authors "xen-api@lists.xen.org")
(maintainers "Xapi project maintainers")
(homepage "https://xapi-project.github.io/")


(package
  (name xapi-sdk)
  (license "BSD-2-Clause")
  (synopsis "Xen API SDK generation code")
  (depends
   (alcotest :with-test)
   astring
   mustache
   (xapi-datamodel (= :version))
   (xapi_stdext_unix (and (= :version) :with-test))
  )
  (allow_empty)
)
(package
  (name xen-api-client-lwt)
)

(package
  (name xen-api-client-async)
)

(package
  (name xen-api-client)
  (synopsis "Xen-API client library for remotely-controlling a xapi host")
  (authors "David Scott" "Anil Madhavapeddy" "Jerome Maloberti" "John Else" "Jon Ludlam" "Thomas Sanders" "Mike McClurg")
  (depends
   dune-build-info
   (alcotest :with-test)
   astring
   base-bigarray
   base-unix
   (cohttp (>= "0.22.0"))
   mtime
   re
   rpclib
   uri
   (xapi-idl (= :version))
   uuidm
   xmlm
  )
)

(package
  (name xe)
)

(package
  (name xapi-types)
)

(package
  (name xapi-tracing)
  (depends
   (xen-api-client (= :version))
  )
  (synopsis "Allows to instrument code to generate tracing information")
  (description "This library provides modules to allow gathering runtime traces.")
)

(package
  (name xapi-tracing-export)
  (depends
   (xen-api-client (= :version))
  )
  (synopsis "Export traces in multiple protocols and formats")
  (description "This library export traces is able to push traces to http endpoints or generate compressed tarballs in the filesystem.")
)

(package
  (name rrdd-plugin)
  (synopsis "A plugin library for the xapi performance monitoring daemon")
  (description "This library allows one to expose a datasource which can then be sampled by the performance monitoring daemon.")
  (depends
   ocaml
   astring
   rpclib
   (rrd-transport (= :version))
   (xapi-stdext-pervasives (= :version))
   (xapi_stdext_std (= :version))
   (xapi-stdext-threads (= :version))
   (xapi_stdext_unix (= :version))
   (xapi-idl (= :version))
   xenstore_transport
  )
)

(package
  (name xapi-idl)
)

(package
  (name xapi-client)
)

(package
  (name xapi)
)

(package
  (name vhd-tool)
)

(package
  (name vhd-format)
)

(package
  (name vhd-format-lwt)
  (synopsis "Lwt interface to read/write VHD format data")
  (description "A pure OCaml library to read and write
[vhd](http://en.wikipedia.org/wiki/VHD_(file_format)) format data, plus a
simple command-line tool which allows vhd files to be interrogated,
manipulated, format-converted and streamed to and from files and remote
servers.

This package provides an Lwt compatible interface to the library.")
  (authors "Jon Ludlam" "Dave Scott")
  (maintainers "Dave Scott <dave@recoil.org>")
  (tags ("org:mirage" "org:xapi-project"))
  (homepage "https://github.com/mirage/ocaml-vhd")
  (source (github mirage/ocaml-vhd))
  (depends
    (ocaml (and (>= "4.02.3") (< "5.0.0")))
    (alcotest :with-test)
    (alcotest-lwt :with-test)
    (cstruct (< "6.1.0"))
    (lwt (>= "3.2.0"))
    (mirage-block (>= "2.0.1"))
    (vhd-format (= :version))
    (io-page (and :with-test (>= "2.4.0")))
  )
)

(package
  (name pciutil)
)

(package
  (name ezxenstore)
)

(package
  (name xapi-rrd)
)
