# ModuLog : Modular Datalog

This is an implementation of Datalog with an OCaml like module system,
including module signatures and higher order modules via functors. The
addition of a module system endows Datalog with two previously missing
features: code reuse and type abstraction. Modulog was motivated by
previous attempts to use the C preprocessor or other text munging to
do hacky code reuse in Datalog for points to analysis.

At the moment Modulog has the following features:

 - Basic Datalog (no negation, aggregation, etc.)
 - An OCaml inspired module system, based on Leroy's [modular
   modules](http://caml.inria.fr/pub/papers/xleroy-modular_modules-jfp.pdf). This
   is implemented in a core-language generic way, so it could
   plausibly be reused for 
 - An bottom-up interpreter that can read input data from CSV files.
 - Compilation to C, currently incomplete (doesn't support reading
   external input yet).

Example:

```
module type Edges = sig
  type vertex

  pred edge : vertex * vertex
end

module MyEdges = struct
  type vertex = int

  define edge : vertex * vertex
    edge(1, 2)
    edge(2, 3)
    edge(3, 4)
    edge(4, 1)
end

module Path (E : Edges) = struct

  type vertex = E.vertex

  define path : E.vertex * E.vertex
    path(?X,?Y) :- E.edge(?X,?Y)
    path(?X,?Z) :- path(?X,?Y), E.edge(?Y,?Z)

end

module P = Path (MyEdges)
```

Still to do:

 - Finish the C output to be able to read in initial data from CSV
   files.
 - More expressive datatype language.

## Related work

- [*A calculus for the construction of modular prolog
  programs*](https://www.sciencedirect.com/science/article/pii/0743106692900422)
  by Donald T. Sannella and Lincoln A. Wallen, J. Logic Programming,
  vol 12, issues 1-2, January 1992, pages 147-177.

- [*QL: Object-oriented Queries on Relational
  Data*](https://doi.org/10.4230/LIPIcs.ECOOP.2016.2) by Pavel
  Avgustinov, Oege de Moor, Michael Peyton Jones, and Max Schäfer,
  ECOOP 2016: 2:1-2:25. DOI: 10.4230/LIPIcs.ECOOP.2016.2
