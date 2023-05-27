module IntMap = Map.Make (Int)
module SymbolMap = Map.Make (Automaton.Symbol)

let lib =
  "  let lexfun = ref (fun _ -> assert false)\n\
  \  let lexbuf = ref (Lexing.from_string String.empty)\n\
  \  let peeked = ref None\n\
  \  let lexbuf_fallback_p = ref Lexing.dummy_pos\n\n\
  \  let setup lf lb =\n\
  \    lexfun := lf;\n\
  \    lexbuf := lb;\n\
  \    peeked := None;\n\
  \    lexbuf_fallback_p := !lexbuf.lex_curr_p\n\
  \  ;;\n\n\
  \  let shift () =\n\
  \    let loc, _ = Option.get !peeked in\n\
  \    peeked := None;\n\
  \    lexbuf_fallback_p := !lexbuf.lex_curr_p;\n\
  \    loc\n\
  \  ;;\n\n\
  \  let lookahead () =\n\
  \    match !peeked with\n\
  \    | Some (_, tok) -> tok\n\
  \    | None ->\n\
  \      let tok = !lexfun !lexbuf\n\
  \      and loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in\n\
  \      peeked := Some (loc, tok);\n\
  \      tok\n\
  \  ;;\n\n\
  \  let reduce_loc ~loc = function\n\
  \    | 0 -> (!lexbuf_fallback_p, !lexbuf_fallback_p) :: loc\n\
  \    | n ->\n\
  \      let rec skip n xs = if n = 0 then xs else skip (n - 1) (List.tl xs) in\n\
  \      let l = fst (List.nth loc (n - 1)), snd (List.hd loc) in\n\
  \      l :: skip n loc\n\
  \  ;;\n\n"
;;

module Make (S : Types.Settings) (G : Types.Grammar) (A : Types.Automaton) : Types.Code =
struct
  open Automaton
  module D = Graphviz.Make (G)

  let term_name t = (G.term t).ti_name.data
  let nterm_name n = (G.nterm n).ni_name.data

  let symbol_name = function
    | Term t -> term_name t
    | NTerm n -> nterm_name n
  ;;

  let symbol_has_value = function
    | NTerm _ -> true
    | Term t -> (G.term t).ti_ty |> Option.is_some
  ;;

  let indent s i =
    String.trim s
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> String.concat ("\n" ^ i)
  ;;

  let letrec ?(pre = "let rec") ?(pre' = "and") ?(post = "") ?(post' = " in") f xs =
    let rec loop i = function
      | [] -> ()
      | x :: xs ->
        f i x (if i = 0 then pre else pre') (if xs = [] then post' else post);
        loop (i + 1) xs
    in
    loop 0 xs
  ;;

  let write_line_directive f (loc, _) =
    Format.fprintf
      f
      "\n# %d \"%s\"\n%s"
      loc.Lexing.pos_lnum
      loc.Lexing.pos_fname
      (String.make (loc.pos_cnum - loc.pos_bol) ' ')
  ;;

  let write_string f { loc; data } =
    if S.line_directives
    then Format.fprintf f "%t%s" (fun f -> write_line_directive f loc) data
    else Format.fprintf f "%s" (String.trim data)
  ;;

  let write_arg_id f symbol idx =
    if S.readable_ids
    then Format.fprintf f "a%d_%s" idx (symbol_name symbol)
    else Format.fprintf f "a%d" idx
  ;;

  (* Continuations are prefixed with underscore because
     precedence declarations could make them unused
     (see unary minus in `calc/ParserPres.mly`) *)
  let write_cont_id f group idx =
    match S.readable_ids, group.g_starting with
    | false, _ -> Format.fprintf f "_c%d" idx
    | true, false -> Format.fprintf f "_c%d_%s" idx (nterm_name group.g_symbol)
    | true, true -> Format.fprintf f "_c%d_%s_starting" idx (nterm_name group.g_symbol)
  ;;

  let write_semantic_action_id f action idx =
    if S.readable_ids
    then Format.fprintf f "a%d_%s" idx (nterm_name action.sa_symbol)
    else Format.fprintf f "a%d" idx
  ;;

  let write_state_id f idx =
    if S.readable_ids then Format.fprintf f "state_%d" idx else Format.fprintf f "s%d" idx
  ;;

  let write_cont_ids f p groups =
    let iter i g = if p g then Format.fprintf f " %t" (fun f -> write_cont_id f g i) in
    List.iteri iter groups
  ;;

  let write_arg_ids f symbols =
    let iter i s =
      if symbol_has_value s then Format.fprintf f " %t" (fun f -> write_arg_id f s i)
    in
    List.iteri iter symbols
  ;;

  let write_term_pattern f bind t =
    if symbol_has_value (Term t)
    then Format.fprintf f "%s %s" (term_name t) (if bind then "x" else "_")
    else Format.fprintf f "%s" (term_name t)
  ;;

  let write_term_patterns f ts =
    let f sym = Format.fprintf f "| %t " (fun f -> write_term_pattern f false sym) in
    TermSet.iter f ts
  ;;

  let write_goto_call f state sym =
    let closure = state.s_kernel @ state.s_closure in
    write_state_id f (SymbolMap.find sym state.s_goto);
    Format.fprintf f " ~loc";
    if symbol_has_value sym then Format.fprintf f " x";
    write_arg_ids f (List.find (shifts_group sym) closure).g_prefix;
    write_cont_ids f (shifts_group sym) (state.s_kernel @ state.s_closure)
  ;;

  let write_cont_definition f state group idx =
    let sym = NTerm group.g_symbol in
    Format.fprintf
      f
      "%t ~loc x = %t"
      (fun f -> write_cont_id f group idx)
      (fun f -> write_goto_call f state sym)
  ;;

  let write_semantic_action_call f group = function
    | { i_action = -1; _ } ->
      assert (List.length group.g_prefix = 1);
      write_arg_ids f group.g_prefix
    | { i_action; _ } ->
      let action = IntMap.find i_action A.automaton.a_actions in
      Format.fprintf
        f
        " Actions.%t ~loc%t ()"
        (fun f -> write_semantic_action_id f action i_action)
        (fun f -> write_arg_ids f group.g_prefix)
  ;;

  let write_action_shift f state sym =
    if S.comments then Format.fprintf f "    (* Shift *)\n";
    Format.fprintf
      f
      "    | %t ->\n      let loc = shift () :: loc in\n      %t\n"
      (fun f -> write_term_pattern f true sym)
      (fun f -> write_goto_call f state (Term sym))
  ;;

  let write_action f state lookahead = function
    | Shift -> TermSet.iter (write_action_shift f state) lookahead
    | Reduce (i, j) ->
      if S.comments then Format.fprintf f "    (* Reduce *)\n";
      let group = List.nth (state.s_kernel @ state.s_closure) i in
      let n = List.length group.g_prefix
      and item = List.nth group.g_items j in
      Format.fprintf
        f
        "    %t->\n\
        \      let x =%t\n\
        \      and loc = reduce_loc ~loc %d in\n\
        \      %t ~loc x\n"
        (fun f -> write_term_patterns f lookahead)
        (fun f -> write_semantic_action_call f group item)
        n
        (fun f -> write_cont_id f group i)
  ;;

  let write_actions f id state =
    Format.fprintf f "    match lookahead () with\n";
    List.iter (fun (l, m) -> write_action f state l m) state.s_action;
    Format.fprintf f "    | _ -> raise (Failure \"error in state %d\")\n" id
  ;;

  let write_actions_starting f state =
    if S.comments then Format.fprintf f "    (* Reduce *)\n";
    let group = List.hd state.s_kernel in
    let item = List.nth group.g_items 0 in
    Format.fprintf
      f
      "    let x =%t in\n    %t x\n"
      (fun f -> write_semantic_action_call f group item)
      (fun f -> write_cont_id f group 0)
  ;;

  let write_term_cons f = function
    | { ti_name; ti_ty = None; _ } ->
      Format.fprintf f "  | %t\n" (fun f -> write_string f ti_name)
    | { ti_name; ti_ty = Some ty; _ } ->
      Format.fprintf
        f
        "  | %t of (%t)\n"
        (fun f -> write_string f ti_name)
        (fun f -> write_string f ty)
  ;;

  let write_term_type f symbols =
    let get_info = function
      | NTerm _ -> None
      | Term t -> Some (G.term t)
    and cmp a b = String.compare b.ti_name.data a.ti_name.data in
    let infos = List.filter_map get_info symbols in
    let infos = List.fast_sort cmp infos in
    Format.fprintf f "type token =\n";
    List.iter (write_term_cons f) infos;
    Format.fprintf f "\n"
  ;;

  let write_semantic_action f id action =
    let item = List.nth (G.group action.sa_symbol).g_items action.sa_index in
    let iter s = function
      | _ when symbol_has_value s = false -> ()
      | Some a -> Format.fprintf f " %s" a
      | None -> Format.fprintf f " _"
    in
    write_semantic_action_id f action id;
    Format.fprintf f " ~loc:_loc";
    List.iter2 iter (List.rev item.i_suffix) (List.rev action.sa_args);
    Format.fprintf f " () = %t" (fun f -> write_string f action.sa_code)
  ;;

  let write_state_comment f state =
    let ci = Format.asprintf "%a" D.fmt_state state
    and cs = Format.asprintf "%a" D.fmt_state_shifts state
    and ca = Format.asprintf "%a" D.fmt_state_actions state in
    Format.fprintf
      f
      "  (* ITEMS:\n       %s\n     GOTO:\n       %s\n     ACTION:\n       %s *)\n"
      (indent ci "       ")
      (indent cs "       ")
      (indent ca "       ")
  ;;

  let write_state_sig f id state =
    Format.fprintf
      f
      "%t ~loc%t%t =\n"
      (fun f -> write_state_id f id)
      (fun f -> write_arg_ids f (List.hd state.s_kernel).g_prefix)
      (fun f -> write_cont_ids f (fun _ -> true) state.s_kernel)
  ;;

  let write_state_body f id state =
    let kn = List.length state.s_kernel in
    let gen_state_cont_def i group pre post =
      let fc f = write_cont_definition f state group (i + kn) in
      Format.fprintf f "    %s %t%s\n" pre fc post
    in
    letrec gen_state_cont_def state.s_closure;
    let group = List.hd state.s_kernel in
    if group.g_starting && (List.hd group.g_items).i_suffix = []
    then write_actions_starting f state
    else write_actions f id state
  ;;

  let write_state f id state =
    write_state_sig f id state;
    write_state_body f id state
  ;;

  let write_entry f symbol id =
    Format.fprintf
      f
      "let %s lexfun lexbuf =\n\
      \  States.setup lexfun lexbuf;\n\
      \  States.%t ~loc:[] (fun x -> x)\n\
       ;;\n"
      (nterm_name symbol)
      (fun f -> write_state_id f id)
  ;;

  let write f =
    let write_semantic_action f id a =
      Format.fprintf f "  let %t\n" (fun f -> write_semantic_action f id a)
    and write_state f _ (id, s) pre post =
      if S.comments then write_state_comment f s;
      Format.fprintf f "  %s %t%s" pre (fun f -> write_state f id s) post
    and write_entry f (nt, s) = write_entry f nt s
    and state_letrec = letrec ~post:"\n" ~post':"  ;;\n" in
    (* -unused-rec-flag due continuations always being mutually recursive, while often they don't need to *)
    (* FIXME: should we include -redunant-{case, subpat}? They trigger warnings
       in grammars with unresolved conflicts, but maybe it's a good thing? *)
    Format.fprintf
      f
      "[@@@@@@warning \"-unused-rec-flag\"]\n\
       [@@@@@@warning \"-redundant-case\"]\n\
       [@@@@@@warning \"-redundant-subpat\"]\n\n\
       %t\n\n\
       %tmodule Actions = struct\n\
       %tend\n\n\
       module States = struct\n\
       %s%tend\n\n\
       %t"
      (fun f -> write_string f A.automaton.a_header)
      (fun f -> write_term_type f G.symbols)
      (fun f -> IntMap.iter (write_semantic_action f) A.automaton.a_actions)
      lib
      (fun f -> IntMap.bindings A.automaton.a_states |> state_letrec (write_state f))
      (fun f -> List.iter (write_entry f) A.automaton.a_starting)
  ;;
end
