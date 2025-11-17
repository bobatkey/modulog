(lang dune 3.14)
(using menhir 2.1)

(name modulog)

; (generate_opam_files true)

(package
 (name modulog-bin)
 (depends cmdliner fmt ppx_monoid ocamlgraph menhir menhirLib))

(package
 (name datalog)
 (depends ocamlgraph fmt))

(package
 (name idealised_algol)
 (depends fmt ppx_monoid))

(package
 (name modulog)
 (depends menhirLib fmt menhir))

(package
 (name relation_machine)
 (depends ocamlgraph fmt))

(package
 (name modules))

(package
 (name display_names))
