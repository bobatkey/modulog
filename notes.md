# Notes


- On fast large-scale program analysis in Datalog
  http://dl.acm.org/citation.cfm?id=2892226

- Soufflé: On Synthesis of Program Analyzers
  http://link.springer.com/chapter/10.1007/978-3-319-41540-6_23
  https://github.com/oracle/souffle

- https://blog.acolyer.org/2018/03/27/anna-a-kvs-for-any-scale/

## Code generation

- A C++ library for generating machine code:

    https://github.com/asmjit/asmjit

  This was mentioned in a sequence of blog posts on making a JIT for
  Brainfuck:

    http://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/
    http://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-2-an-x64-jit/


## Using relational data stores

- "SQLGraph: An Efficient Relational-Based Property Graph Store" is a
  paper describing efficient ways of storing graph structured data in
  a relational database.

    https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/43287.pdf


## B-Trees

- https://github.com/tomjridge/tjr_btree
  A B-Tree implementation in OCaml, extracted from Isabelle/HOL. By Tom Ridge.

- https://github.com/spacejam/rsdb/blob/master/README.md
  A Rust implementation of something called "BW-Trees", which apparently work better on SSDs

# Compilation

1. Alter `Relation_machine.of_rules` to avoid unnecessary copying by
   unrolling the fixpoint loops by one.
   
2. Do a naive translation that ignores indexes.

3. Then include the indexes


# Other ideas

- Conversion of regular expressions into rules describing an automaton?
  - 
- Streaming predicates
- Conversion of LTL formulae into Buchi automata?
- Greatest fixpoints; Inf-datalog; CTL.
- Proof producing execution
- Semiring weighted relations?
- Negation, and other aggregation operators
- Proper integration of external data sources as extensional dbs
  - And connect these into the interpreter(s) and compiler
- Modules: Signature update; `open`; `include`; `module type of`;
  separate compilation.

# Datalog optimisations

## (Non-size-increasing) Inlining

If there is a rule of the form:

    a(?X1,...,?Xn) :- b(?Xi1, ..., ?Xim)
    
and this is the only rule defining `a`, then replace all occurences of
`a` with the appropriate instantiation of `b`.

More generally, might do partial evaluation of the datalog? to
eliminate fully ground rules. This would require duplication of
existing rules.

## (potentially) join re-ordering?

## Magic sets?

- Given a top-down pattern
- Essentially, push through the requirements to guide the bottom-up
  solver.

# Module systems

- A caml-list post by Andreas Rossberg on how checking of module
  signature subsumption in OCaml is undecidable due to abstract module
  signatures:

    http://caml.inria.fr/pub/old_caml_site/caml-list/1507.html

- Rossberg's "1ML – Core and Modules United (F-ing First-class
  Modules)" describes a new ML-like language where the core language
  and the module language are unified. Based on the "F-ing modules"
  translation from modules for ML to System Fomega.

- Harper and Stone's "A Type-Theoretic Interpretation of Standard
  ML". An approach to understanding SML's module system by seeing it
  as a kind of dependent type theory.

- Dreyer's PhD thesis "Understanding and Evolving the ML Module
  System". Presents a survey of existing ways of describing the ML
  module system and attempts to unify them in a single common
  system. Uses this unification to extend ML modules to include
  recursive modules. Proposes a new form of ML based on this work.
