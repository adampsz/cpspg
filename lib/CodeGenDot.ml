module type DotCode = sig
  include Types.Code

  (* These functions are used in comments in other codegen backends *)
  val fmt_state : Format.formatter -> Automaton.state -> unit
  val fmt_state_shifts : Format.formatter -> Automaton.state -> unit
  val fmt_state_actions : Format.formatter -> Automaton.state -> unit
end

module Make (S : Types.Settings) (G : Types.Grammar) (A : Types.Automaton) : DotCode =
struct
  open Automaton

  let term_name t = (G.term t).ti_name.data
  let nterm_name n = (G.nterm n).ni_name.data

  let fmt_arg f = function
    | Term t -> Format.pp_print_string f (term_name t)
    | NTerm n -> Format.pp_print_string f (nterm_name n)
  ;;

  let fmt_follow f i t =
    Format.fprintf f "%s%s" (if i > 0 then ", " else " \t\t/ ") (term_name t)
  ;;

  let fmt_args f args = List.iter (Format.fprintf f " %a" fmt_arg) args

  let fmt_item f group item =
    let i = if group.g_starting then "'" else ""
    and name = nterm_name group.g_symbol
    and pref = List.rev group.g_prefix
    and suff = item.i_suffix in
    Format.fprintf f "%s%s →%a .%a" name i fmt_args pref fmt_args suff;
    TermSet.to_seq group.g_lookahead |> Seq.iteri (fmt_follow f)
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
    let fmt_action (la, action) =
      TermSet.iter fmt_sym la;
      Format.fprintf f " -> ";
      fmt_state_action action;
      Format.fprintf f "\n"
    in
    List.iter fmt_action state.s_action
  ;;

  let fmt_automaton f automaton =
    let replace c r s = String.split_on_char c s |> String.concat r in
    let escape s = s |> replace '"' "\\\"" |> replace '\n' "\\l" in
    let fmt_action id arg id' =
      let arg = Format.asprintf "%a" fmt_arg arg |> escape in
      Format.fprintf f "  s%d -> s%d [ taillabel = \"%s\"; ];\n" id id' arg
    in
    let fmt_node id s =
      let items = Format.asprintf "%a" fmt_state s |> escape in
      Format.fprintf f "  s%d [ xlabel = s%d; label = \"%s\"; ];\n" id id items;
      SymbolMap.iter (fun arg id' -> fmt_action id arg id') s.s_goto
    in
    Format.fprintf f "digraph g {\n";
    Format.fprintf f "  nodesep = 1; pack = true;\n";
    Format.fprintf f "  node [ shape = box; fontname = monospace ];";
    IntMap.iter fmt_node automaton.a_states;
    Format.fprintf f "}\n"
  ;;

  let write f = fmt_automaton f A.automaton
end
