let dot_of_ruleset fmt ruleset =
  let module G = struct
    include Ruleset.As_graph

    let graph_attributes _ =
      [ ]

    let default_vertex_attributes _ =
      [ `Shape    `Box
      ; `Fontname "Ubuntu Mono"
      ; `Fontsize 8
      ]

    let vertex_name id =
      Printf.sprintf "rule%d" (Ruleset.rule_id id)

    let vertex_attributes id =
      let rule = Ruleset.rule_of_id id ruleset in
      [ `Label (Format.asprintf "@[%a@]" Ruleset.pp_rule rule) ]

    let default_edge_attributes _ = []

    let edge_attributes edge = []

    let get_subgraph _ =
      None
  end in
  let module Dot_of_ruleset = Graph.Graphviz.Dot (G) in
  Dot_of_ruleset.fprint_graph fmt ruleset
