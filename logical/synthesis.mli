val synthesise :
  Checker.TypeChecker.Env.t ->
  'a ->
  Core_syntax.SurfaceSyntax.mod_type ->
  (unit,
   [> `Synth_error of Location.t * string
    | `TypeChecker of Checker.TypeChecker.error ])
  result
