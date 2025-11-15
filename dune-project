(lang dune 3.14)

(name modulog)

(generate_opam_files true)

(package
 (name modulog-bin)
 (depends cmdliner))

(package
 (name datalog)
 (depends ocamlgraph fmt))

(package
 (name idealised_algol)
 (depends fmt ppx_monoid))

(package
 (name modulog)
 (depends menhirLib fmt))

(package
 (name relation_machine)
 (depends ocamlgraph fmt))

(package
 (name modules))

(package
 (name display_names))
