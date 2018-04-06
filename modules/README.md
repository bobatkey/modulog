# Modular Modules

A implementation of Leroy's [modular
modules](http://caml.inria.fr/pub/papers/xleroy-modular_modules-jfp.pdf),
an OCaml like module language parameterised by the implementation of
the core language. Given a core language type checker, this library
adds facilities for modules, module signatures and functors.

With respect to Leroy's paper, this implementation differs in several
ways that make it more practical for a realistic language
implementation:

 - A partial grammar with an OCaml inspired syntax, which can be
   integrated with the core language's grammar via
   [Menhir](http://cristal.inria.fr/~fpottier/menhir/)'s multi-file
   grammar support.
 - Pretty printing of the module language, given a pretty printer for
   the core language.
 - Support for multiple values bindings in a single structure item
   declaration, which allows recursively defined values.
 - Error reporting, with pretty printing of errors and line/column
   numbers.
 - An evaluator, for 'executing' the module language to produce a
   module-free program with all functors instantiated.
 - An implementation of recursive modules, based on ([Leroy's design
   notes for OCaml's recursive
   modules](http://caml.inria.fr/pub/papers/xleroy-recursive_modules-03.pdf)),
   but slighly simpler.

Still to do (no particular order):

 - Multi-file programs (like OCaml's separate compilation support).
