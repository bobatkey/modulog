module Make (IA : Idealised_algol.Syntax.S) : sig
  val write_tuple :
    IA.Stdio.out_ch IA.exp ->
    int32 IA.exp array ->
    IA.comm

  val read_tuple :
    IA.Stdio.in_ch IA.exp ->
    width:int ->
    parsed:(int32 IA.exp array -> IA.comm) ->
    eof:IA.comm ->
    IA.comm
end = struct

  type w = W : { format : 'a IA.Stdio.fmt; cont : 'a -> IA.comm } -> w

  let rec build_writer arr n =
    let open! IA.Stdio in
    if n = Array.length arr - 1 then
      W { format = int32 @@ lit "\n" @@ stop
        ; cont   = (fun k -> k arr.(n))
        }
    else
      let W {format; cont} = build_writer arr (n+1) in
      W { format = int32 @@ lit "," @@ format
        ; cont   = (fun k -> cont (k arr.(n)))
        }

  let write_tuple out_ch arr =
    let open! IA.Stdio in
    let W { format; cont } = build_writer arr 0 in
    printf out_ch format |> cont

  type r = R : { format : 'a IA.Stdio.fmt; cont : int32 IA.exp list -> 'a } -> r

  let rec build_reader n i k =
    let open! IA.Stdio in
    if i = n - 1 then
      R { format = int32 @@ lit "\n" @@ stop
        ; cont   = (fun exps exp -> k (Array.of_list (List.rev (exp::exps))))
        }
    else
      let R {format; cont} = build_reader n (i+1) k in
      R { format = int32 @@ lit "," @@ format
        ; cont   = (fun exps exp -> cont (exp::exps))
        }

  let read_tuple in_ch ~width ~parsed ~eof =
    let open! IA.Stdio in
    let R {format; cont} = build_reader width 0 parsed in
    scanf in_ch format ~parsed:(cont []) ~eof

end
