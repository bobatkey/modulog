open Modlog.Std

let typecheck filename =
  let structure = read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.Mod.pp_signature sg

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let relmachine filename with_indexes =
  let structure = read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Modlog.To_rules.from_structure str in
       let code     = Relation_machine.Of_rules.translate rules in
       Format.printf
         "@[<v>%a@]\n%!"
         Relation_machine.Syntax.pp_program code;
       if with_indexes then begin
         let indexes = Relation_machine.Indexes.indexes code in
         Format.printf
           "\n@[<v>%a@]\n%!"
           Relation_machine.Indexes.pp_all_orderings indexes
       end

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let rules filename =
  let structure = read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules = Modlog.To_rules.from_structure str in
       Format.printf
         "@[<v>%a@]\n"
         Datalog.Ruleset.pp rules

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let rules_graph filename =
  let structure = read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules = Modlog.To_rules.from_structure str in
       Format.printf
         "@[<v>%a@]\n"
         Datalog.Graphviz.dot_of_ruleset rules

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let exec filename =
  let structure = read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Modlog.To_rules.from_structure str in
       let code     = Relation_machine.Of_rules.translate rules in
       let env      = Relation_machine.Interpreter.eval code in
       Format.printf
         "@[<v>%a@]\n"
         Relation_machine.Interpreter.Env.pp env

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

(**********************************************************************)
(* The command line interface *)

open Cmdliner

let filename_arg =
  Arg.(required
       & pos 0 (some string) None
       & info []
         ~docv:"FILENAME"
         ~doc:"Name of modular datalog file to process")

let with_indexes_opt =
  let doc = "Whether to print computed index information" in
  Arg.(value & flag & info ["i";"with-indexes"] ~doc)

let typecheck_cmd =
  let doc = "Typecheck a Modular Datalog program and print the signature" in
  Term.(const typecheck $ filename_arg),
  Term.info "typecheck" ~doc ~exits:Term.default_exits

let relmachine_cmd =
  let doc = "Compile a Modular Datalog program to the RelMachine" in
  Term.(const relmachine $ filename_arg $ with_indexes_opt),
  Term.info "relmachine" ~doc ~exits:Term.default_exits

let rules_cmd =
  let doc = "Compile a Modular Datalog program to flat datalog rules" in
  Term.(const rules $ filename_arg),
  Term.info "rules" ~doc ~exits:Term.default_exits

let rules_graph_cmd =
  let doc = "Compile a Modular Datalog program to a graph of datalog rules" in
  Term.(const rules_graph $ filename_arg),
  Term.info "rules-graph" ~doc ~exits:Term.default_exits

let exec_cmd =
  let doc = "Execute a Modular Datalog program" in
  Term.(const exec $ filename_arg),
  Term.info "exec" ~doc ~exits:Term.default_exits

let default_cmd =
  let doc = "a Modular Datalog compiler" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  (*let man = help_secs in*)
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "modlog" ~version:"v1.0.0" ~doc ~sdocs ~exits (*~man*)

let () =
  Term.(exit (eval_choice default_cmd [ typecheck_cmd
                                      ; relmachine_cmd
                                      ; rules_cmd
                                      ; rules_graph_cmd
                                      ; exec_cmd ]))
