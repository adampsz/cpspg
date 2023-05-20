module IntMap = Map.Make (Int)
module SymbolMap = Map.Make (Automaton.Symbol)

module type Settings = sig
  val comments : bool
  val readable_ids : bool
  val log : ('a, Format.formatter, unit) format -> 'a
end

module type Input = sig
  val f : Format.formatter
  val term : Automaton.Terminal.t -> Automaton.term_info
  val nterm : Automaton.Nonterminal.t -> Automaton.nterm_info
  val group : Automaton.Nonterminal.t -> Automaton.group
  val symbols : Automaton.symbol list
  val automaton : Automaton.t
end

let lib =
  "  let lexfun = ref (fun _ -> assert false)\n\
  \  let lexbuf = ref (Lexing.from_string String.empty)\n\
  \  let lookahead = ref None\n\n\
  \  let setup lf lb =\n\
  \    lexfun := lf;\n\
  \    lexbuf := lb;\n\
  \    lookahead := None\n\
  \  ;;\n\n\
  \  let shift () =\n\
  \    let t = Option.get !lookahead in\n\
  \    lookahead := None;\n\
  \    t\n\
  \  ;;\n\n\
  \  let lookahead () =\n\
  \    match !lookahead with\n\
  \    | Some t -> t\n\
  \    | None ->\n\
  \      let t = !lexfun !lexbuf in\n\
  \      lookahead := Some t;\n\
  \      t\n\
  \  ;;\n\n"
;;

module Run (S : Settings) (I : Input) = struct
  open Automaton
  module D = Graphviz.Make (I)

  let symbol_name = function
    | Term t -> (I.term t).ti_name
    | NTerm n -> (I.nterm n).ni_name
  ;;

  let symbol_has_value = function
    | NTerm _ -> true
    | Term t -> (I.term t).ti_ty |> Option.is_some
  ;;

  let indent s i =
    String.trim s
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> String.concat ("\n" ^ i)
  ;;

  let gen_letrec ?(pre = "let rec") ?(pre' = "and") ?(post = "") ?(post' = " in") f xs =
    let rec loop i = function
      | [] -> ()
      | x :: xs ->
        f i x (if i = 0 then pre else pre') (if xs = [] then post' else post);
        loop (i + 1) xs
    in
    loop 0 xs
  ;;

  let gen_arg_id symbol i =
    if S.readable_ids
    then Format.fprintf I.f "a%d_%s" i (symbol_name symbol)
    else Format.fprintf I.f "a%d" i
  ;;

  let gen_cont_id g i =
    match S.readable_ids, g.g_starting with
    | true, false -> Format.fprintf I.f "c%d_%s" i (symbol_name (NTerm g.g_symbol))
    | true, true ->
      Format.fprintf I.f "c%d_%s_starting" i (symbol_name (NTerm g.g_symbol))
    | false, _ -> Format.fprintf I.f "c%d" i
  ;;

  let gen_semantic_action_id a i =
    if S.readable_ids
    then Format.fprintf I.f "a%d_%s" i (symbol_name (NTerm a.sa_symbol))
    else Format.fprintf I.f "a%d" i
  ;;

  let gen_state_id i =
    if S.readable_ids then Format.fprintf I.f "state_%d" i else Format.fprintf I.f "s%d" i
  ;;

  let gen_cont_ids f groups =
    let iter i g = if f g then Format.fprintf I.f " %t" (fun _ -> gen_cont_id g i) in
    List.iteri iter groups
  ;;

  let gen_arg_ids symbols =
    let iter i s =
      if symbol_has_value s then Format.fprintf I.f " %t" (fun _ -> gen_arg_id s i)
    in
    List.iteri iter symbols
  ;;

  let gen_token_pat x t =
    if symbol_has_value (Term t)
    then Format.fprintf I.f "%s %s" (symbol_name (Term t)) x
    else Format.fprintf I.f "%s" (symbol_name (Term t))
  ;;

  let gen_token_pats lookahead =
    let f sym = Format.fprintf I.f "| %t " (fun _ -> gen_token_pat "_" sym) in
    TermSet.iter f lookahead
  ;;

  let gen_shift state sym =
    gen_state_id (SymbolMap.find sym state.s_goto);
    if symbol_has_value sym then Format.fprintf I.f " x";
    gen_arg_ids (List.find (shifts_group sym) (state.s_kernel @ state.s_closure)).g_prefix;
    gen_cont_ids (shifts_group sym) (state.s_kernel @ state.s_closure)
  ;;

  let gen_cont_def i state group =
    let sym = NTerm group.g_symbol in
    Format.fprintf
      I.f
      "%t x = %t"
      (fun _ -> gen_cont_id group i)
      (fun _ -> gen_shift state sym)
  ;;

  let gen_action_call group = function
    | { i_action = 0; _ } ->
      assert (List.length group.g_prefix = 1);
      gen_arg_ids group.g_prefix
    | { i_action; _ } ->
      let action = IntMap.find i_action I.automaton.a_actions in
      Format.fprintf I.f " Actions.%t" (fun _ -> gen_semantic_action_id action i_action);
      gen_arg_ids group.g_prefix;
      Format.fprintf I.f " ()"
  ;;

  let gen_action_shift state sym =
    if S.comments then Format.fprintf I.f "    (* Shift *)\n";
    Format.fprintf I.f "    | %t ->\n" (fun _ -> gen_token_pat "x" sym);
    Format.fprintf I.f "      let _ = shift () in\n";
    Format.fprintf I.f "      %t\n" (fun _ -> gen_shift state (Term sym))
  ;;

  let gen_action state lookahead = function
    | Shift -> TermSet.iter (gen_action_shift state) lookahead
    | Reduce (i, j) ->
      if S.comments then Format.fprintf I.f "    (* Reduce *)\n";
      let group = List.nth (state.s_kernel @ state.s_closure) i in
      let item = List.nth group.g_items j in
      Format.fprintf I.f "    %t->\n" (fun _ -> gen_token_pats lookahead);
      Format.fprintf I.f "      let x =%t in\n" (fun _ -> gen_action_call group item);
      Format.fprintf I.f "      %t x\n" (fun _ -> gen_cont_id group i)
  ;;

  let gen_state_sig id state =
    gen_state_id id;
    gen_arg_ids (List.hd state.s_kernel).g_prefix;
    gen_cont_ids (fun _ -> true) state.s_kernel;
    Format.fprintf I.f " =\n"
  ;;

  let gen_state_match id state =
    Format.fprintf I.f "    match lookahead () with\n";
    List.iter (fun (l, m) -> gen_action state l m) state.s_action;
    Format.fprintf I.f "    | _ -> raise (Failure \"error in state %d\")\n" id
  ;;

  let gen_state_match_starting state =
    if S.comments then Format.fprintf I.f "    (* Reduce *)\n";
    let group = List.hd state.s_kernel in
    let item = List.nth group.g_items 0 in
    Format.fprintf I.f "    let x =%t in\n" (fun _ -> gen_action_call group item);
    Format.fprintf I.f "    %t x\n" (fun _ -> gen_cont_id group 0)
  ;;

  let gen_state_body id state =
    let kn = List.length state.s_kernel in
    let gen_state_cont_def i items pre post =
      let fc _ = gen_cont_def (i + kn) state items in
      Format.fprintf I.f "    %s %t%s\n" pre fc post
    in
    gen_letrec gen_state_cont_def state.s_closure;
    let group = List.hd state.s_kernel in
    if group.g_starting && (List.hd group.g_items).i_suffix = []
    then gen_state_match_starting state
    else gen_state_match id state
  ;;

  let gen_state_comment state =
    let ci = Format.asprintf "%a" D.fmt_state state
    and cs = Format.asprintf "%a" D.fmt_state_shifts state
    and ca = Format.asprintf "%a" D.fmt_state_actions state in
    Format.fprintf
      I.f
      "  (* ITEMS:\n       %s\n     GOTO:\n       %s\n     ACTION:\n       %s *)\n"
      (indent ci "       ")
      (indent cs "       ")
      (indent ca "       ")
  ;;

  let gen_state id state =
    gen_state_sig id state;
    gen_state_body id state
  ;;

  let gen_semantic_action id action =
    let item = List.nth (I.group action.sa_symbol).g_items action.sa_index in
    let f s = function
      | _ when symbol_has_value s = false -> ()
      | Some a -> Format.fprintf I.f " %s" a
      | None -> Format.fprintf I.f " _"
    in
    gen_semantic_action_id action id;
    List.iter2 f (List.rev item.i_suffix) (List.rev action.sa_args);
    Format.fprintf I.f " () = %s" (String.trim action.sa_code)
  ;;

  let gen_token = function
    | { ti_name; ti_ty = Some ty } -> Format.fprintf I.f "  | %s of %s\n" ti_name ty
    | { ti_name; ti_ty = None } -> Format.fprintf I.f "  | %s\n" ti_name
  ;;

  let gen_tokens symbols =
    let gen_variant = function
      | NTerm _ -> ()
      | Term t -> gen_token (I.term t)
    in
    Format.fprintf I.f "type token =\n";
    List.iter gen_variant symbols;
    Format.fprintf I.f "\n"
  ;;

  let gen_entry (symbol, id) =
    Format.fprintf I.f "let %s lexbuf lexfun =\n" (symbol_name (NTerm symbol));
    Format.fprintf I.f "  States.setup lexfun lexbuf;\n";
    Format.fprintf I.f "  States.%t (fun x -> x)\n" (fun _ -> gen_state_id id);
    Format.fprintf I.f ";;\n"
  ;;

  let _ =
    let gen_semantic_action id a =
      Format.fprintf I.f "  let %t\n" (fun _ -> gen_semantic_action id a)
    and gen_state _ (id, s) pre post =
      if S.comments then gen_state_comment s;
      Format.fprintf I.f "  %s %t%s" pre (fun _ -> gen_state id s) post
    and header = indent I.automaton.a_header "" in
    Format.fprintf I.f "[@@@@@@warning \"-unused-rec-flag\"]\n\n";
    Format.fprintf I.f "%s\n\n" header;
    gen_tokens I.symbols;
    Format.fprintf I.f "module Actions = struct\n";
    IntMap.iter gen_semantic_action I.automaton.a_actions;
    Format.fprintf I.f "end\n\n";
    Format.fprintf I.f "module States = struct\n%s" lib;
    IntMap.bindings I.automaton.a_states
    |> gen_letrec ~post:"\n" ~post':"  ;;\n" gen_state;
    Format.fprintf I.f "end\n\n";
    List.iter gen_entry I.automaton.a_starting
  ;;
end
