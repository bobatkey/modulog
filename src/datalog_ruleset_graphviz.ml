module RS = Datalog_ruleset

module G = struct
  include RS.G

  let graph_attributes rules =
    [ ]

  let default_vertex_attributes rules =
    [ `Shape `Box; `Fontname "Ubuntu Mono"; `Fontsize 8 ]

  let vertex_name (_, id) =
    Printf.sprintf "rule%d" (RS.rule_id id)

  let vertex_attributes (rules, id) =
    [ `Label (Format.asprintf "%a" RS.pp_rule (RS.rule id rules)) ]

  let default_edge_attributes rules = []

  let edge_attributes edge = []

  let get_subgraph _ =
    None
end

module Dot_of_ruleset = Graph.Graphviz.Dot (G)

let dot_of_ruleset =
  Dot_of_ruleset.fprint_graph
