(lang dune 3.7)

(name cpspg)

(generate_opam_files true)

(authors "Adam Szeruda")
(maintainers "Adam Szeruda")
(homepage "https://github.com/adampsz/cpspg")
(bug_reports "https://github.com/adampsz/cpspg/issues")
(license MIT)

(package
 (name cpspg)
 (synopsis "Continuation-passing style parser generator")
 (depends ocaml dune (alcotest :with-test)))
