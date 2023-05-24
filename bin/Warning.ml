let report ?loc tag msg =
  (match loc with
   | Some { Lexing.pos_fname = f; pos_lnum = l; pos_cnum = c; pos_bol = b } ->
     Format.eprintf "\027[1mFile \"%s\", line %d, character %d\027[0m:\n" f l (c - b)
   | None -> ());
  Format.eprintf "%s: %t\n%!" tag msg
;;

let report_err ?loc msg = report ?loc "\027[1;31mError\027[0m" msg
let report_warn ?loc msg = report ?loc "\027[1;35mWarning\027[0m" msg

let report_conflict (module G : Cpspg.Types.Grammar) (module A : Cpspg.Types.Automaton) =
  let open Cpspg.Automaton in
  fun id sym actions ->
    let seen = Hashtbl.create 16 in
    let rec walk path = function
      | i when i = id -> Some path
      | i when Hashtbl.mem seen i -> None
      | i ->
        Hashtbl.add seen i ();
        let state = IntMap.find i A.automaton.a_states in
        SymbolMap.to_seq state.s_goto |> Seq.find_map (fun (s, i) -> walk (s :: path) i)
    in
    let path =
      List.find_map (fun (_, i) -> walk [] i) A.automaton.a_starting
      |> Option.value ~default:[]
    in
    let pp_sym f = function
      | Term t -> Format.fprintf f " %s" (G.term t).ti_name.data
      | NTerm n -> Format.fprintf f " %s" (G.nterm n).ni_name.data
    in
    let pp_path f = function
      | [] -> Format.fprintf f "on empty input"
      | p -> Format.fprintf f "after reading%t" (fun f -> List.iter (pp_sym f) p)
    and pp_action f sym = function
      | Shift -> Format.fprintf f "\n  - shift%t" (fun f -> pp_sym f (Term sym))
      | Reduce (i, j) ->
        let state = IntMap.find id A.automaton.a_states in
        let group = List.nth (state.s_kernel @ state.s_closure) i in
        let item = List.nth group.g_items j in
        Format.fprintf
          f
          "\n  - reduce%t ->%t"
          (fun f -> pp_sym f (NTerm group.g_symbol))
          (fun f -> List.iter (pp_sym f) item.i_suffix)
    in
    report_err (fun f ->
      Format.fprintf
        f
        "Conflict %t in state %d:%t"
        (fun f -> pp_path f path)
        id
        (fun f -> List.iter (pp_action f sym) actions))
;;

let report_conflicts (module G : Cpspg.Types.Grammar) (module A : Cpspg.Types.Automaton) c
  =
  List.iter (fun (i, s, a) -> report_conflict (module G) (module A) i s a) c
;;
