module Make (I : sig
  val term : Automaton.Terminal.t -> Automaton.term_info
  val nterm : Automaton.Nonterminal.t -> Automaton.nterm_info
end) =
struct
  open Automaton

  let term_name t = (I.term t).ti_name
  let nterm_name n = (I.nterm n).ni_name

  let fmt_arg f = function
    | Term t -> Format.pp_print_string f (term_name t)
    | NTerm n -> Format.pp_print_string f (nterm_name n)
  ;;

  let fmt_follow f t = Format.fprintf f " /%s" (term_name t)
  let fmt_args f args = List.iter (Format.fprintf f " %a" fmt_arg) args

  let fmt_item f group item =
    let i = if group.g_starting then "'" else ""
    and name = nterm_name group.g_symbol
    and pref = List.rev group.g_prefix
    and suff = item.i_suffix in
    Format.fprintf f "%s%s →%a .%a" name i fmt_args pref fmt_args suff;
    TermSet.iter (fmt_follow f) group.g_lookahead
  ;;

  let fmt_group f group =
    List.iter (Format.fprintf f "%a\n" (fun f -> fmt_item f group)) group.g_items
  ;;

  let fmt_state f state = List.iter (fmt_group f) (state.s_kernel @ state.s_closure)

  let fmt_state_shifts f state =
    let fmt_shift s id = Format.fprintf f "%a -> %d\n" fmt_arg s id in
    SymbolMap.iter fmt_shift state.s_goto
  ;;

  let fmt_state_actions f state =
    let fmt_sym t = Format.fprintf f " %s" (term_name t)
    and fmt_state_action = function
      | Shift -> Format.fprintf f "shift"
      | Reduce (i, j) -> Format.fprintf f "reduce %d %d" i j
    in
    let fmt_move (la, move) =
      TermSet.iter fmt_sym la;
      Format.fprintf f " -> ";
      fmt_state_action move;
      Format.fprintf f "\n"
    in
    List.iter fmt_move state.s_action
  ;;

  let fmt_automaton f automaton =
    let replace c r s = String.split_on_char c s |> String.concat r in
    let escape s = s |> replace '"' "\\\"" |> replace '\n' "\\l" in
    let fmt_move id arg id' =
      let arg = Format.asprintf "%a" fmt_arg arg |> escape in
      Format.fprintf f "  s%d -> s%d [ taillabel = \"%s\"; ];\n" id id' arg
    in
    let fmt_node id s =
      let items = Format.asprintf "%a" fmt_state s |> escape in
      Format.fprintf f "  s%d [ xlabel = s%d; label = \"%s\"; ];\n" id id items;
      SymbolMap.iter (fun arg id' -> fmt_move id arg id') s.s_goto
    in
    Format.fprintf f "digraph g {\n";
    Format.fprintf f "  nodesep = 1; pack = true;\n";
    Format.fprintf f "  node [ shape = box; fontname = monospace ];";
    IntMap.iter fmt_node automaton.a_states;
    Format.fprintf f "}\n"
  ;;
end
