module IntMap = Map.Make (Int)
module SymbolMap = Map.Make (Automaton.Symbol)

(* -unused-rec-flag due continuations always being mutually recursive, while often they don't need to *)
(* FIXME: should we include -redunant-{case, subpat}? They trigger warnings
   in grammars with unresolved conflicts, but maybe it's a good thing? *)
let prelude =
  "[@@@warning \"-unused-rec-flag\"]\n\
   [@@@warning \"-redundant-case\"]\n\
   [@@@warning \"-redundant-subpat\"]\n"
;;

let action_lib =
  "  let _kw_endpos ~loc _ =\n\
  \    match loc with\n\
  \    | l :: _ -> snd l\n\
  \    | [] -> Lexing.dummy_pos\n\
  \  ;;\n\n\
  \  let _kw_startpos ~loc n =\n\
  \    match List.nth_opt loc (n - 1) with\n\
  \    | Some l -> fst l\n\
  \    | None -> _kw_endpos ~loc n\n\
  \  ;;\n\n\
  \  let _kw_symbolstartpos ~loc:_ _ = failwith \"unimplemented: $symbolstartpos\"\n\
  \  let _kw_startofs ~loc:_ _ = failwith \"unimplemented: $startofs\"\n\
  \  let _kw_endofs ~loc:_ _ = failwith \"unimplemented: $endofs\"\n\
  \  let _kw_symbolstartofs ~loc:_ _ = failwith \"unimplemented: $symbolstartofs\"\n\
  \  let _kw_loc ~loc n = _kw_startpos ~loc n, _kw_endpos ~loc n\n\
  \  let _kw_sloc ~loc:_ _ = failwith \"unimplemented: $sloc\"\n"
;;

let state_lib =
  "  let lexfun = ref (fun _ -> assert false)\n\
  \  let lexbuf = ref (Lexing.from_string String.empty)\n\
  \  let peeked = ref None\n\
  \  let lexbuf_fallback_p = ref Lexing.dummy_pos\n\
  \  let error_token = ref None\n\
  \  let expected_tokens = ref [] \n\n\
  \  let setup lf lb =\n\
  \    lexfun := lf;\n\
  \    lexbuf := lb;\n\
  \    peeked := None;\n\
  \    lexbuf_fallback_p := !lexbuf.lex_curr_p;\n\
  \    error_token := None;\n\
  \    expected_tokens := []\n\
  \  ;;\n\n\
  \  let shift () =\n\
  \    let sym = Option.get !peeked in\n\
  \    peeked := None;\n\
  \    lexbuf_fallback_p := !lexbuf.lex_curr_p;\n\
  \    sym\n\
  \  ;;\n\n\
  \  let peek () =\n\
  \    match !peeked with\n\
  \    | Some p -> p\n\
  \    | None ->\n\
  \      let tok = !lexfun !lexbuf\n\
  \      and loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in\n\
  \      peeked := Some (tok, loc);\n\
  \      tok, loc\n\
  \    ;;\n\n\
  \  let lookahead () = fst (peek ())\n\n\
  \  let fail expected =\n\
  \      let token, _ = peek () in\n\
  \      error_token := Some token;\n\
  \      expected_tokens := expected;\n\
  \      raise Parsing.Parse_error\n\
  \    ;;\n\n\
  \  let loc_shift ~loc l = l :: loc\n\n\
  \  let loc_reduce ~loc = function\n\
  \    | 0 -> (!lexbuf_fallback_p, !lexbuf_fallback_p) :: loc\n\
  \    | n ->\n\
  \      let rec skip n xs = if n = 0 then xs else skip (n - 1) (List.tl xs) in\n\
  \      let l = fst (List.nth loc (n - 1)), snd (List.hd loc) in\n\
  \      l :: skip n loc\n\
  \  ;;\n"
;;

let epilogue =
  "let error_token () = !States.error_token\n\
   let expected_tokens () = !States.expected_tokens\n"
;;

let iteri2 f xs ys =
  let f i x y =
    f i x y;
    i + 1
  in
  List.fold_left2 f 0 xs ys |> ignore
;;

module Make (S : Types.Settings) (G : Types.Grammar) (A : Types.Automaton) : Types.Code =
struct
  open Automaton
  module D = CodeGenDot.Make (S) (G) (A)

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

  let write_term_names f terms =
    let write i t =
      if i > 0 then Format.fprintf f "; ";
      Format.fprintf f "\"%s\"" (term_name t)
    in
    List.iteri write terms
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
    if S.locations then Format.fprintf f " ~loc";
    if symbol_has_value sym then Format.fprintf f " x";
    write_arg_ids f (List.find (shifts_group sym) closure).g_prefix;
    write_cont_ids f (shifts_group sym) (state.s_kernel @ state.s_closure)
  ;;

  let write_cont_definition f state group idx =
    let sym = NTerm group.g_symbol in
    Format.fprintf
      f
      "%t%s x = %t"
      (fun f -> write_cont_id f group idx)
      (if S.locations then " ~loc" else "")
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
        " Actions.%t%s%t ()"
        (fun f -> write_semantic_action_id f action i_action)
        (if S.locations then " ~loc" else "")
        (fun f -> write_arg_ids f group.g_prefix)
  ;;

  let write_action_shift f state sym =
    let write_loc_update f = Format.fprintf f " in\n      let loc = loc_shift ~loc _l" in
    if S.comments then Format.fprintf f "    (* Shift *)\n";
    Format.fprintf
      f
      "    | %t ->\n      let _, _l = shift ()%t in\n      %t\n"
      (fun f -> write_term_pattern f true sym)
      (fun f -> if S.locations then write_loc_update f)
      (fun f -> write_goto_call f state (Term sym))
  ;;

  let write_action_reduce f state lookahead i j =
    let write_loc_update f n =
      Format.fprintf f "\n      and loc = loc_reduce ~loc %d" n
    in
    if S.comments then Format.fprintf f "    (* Reduce *)\n";
    let group = List.nth (state.s_kernel @ state.s_closure) i in
    let n = List.length group.g_prefix
    and item = List.nth group.g_items j in
    Format.fprintf
      f
      "    %t->\n      let x =%t%t in\n      %t%s x\n"
      (fun f -> write_term_patterns f lookahead)
      (fun f -> write_semantic_action_call f group item)
      (fun f -> if S.locations then write_loc_update f n)
      (fun f -> write_cont_id f group i)
      (if S.locations then " ~loc" else "")
  ;;

  let write_action f state lookahead = function
    | Shift -> TermSet.iter (write_action_shift f state) lookahead
    | Reduce (i, j) -> write_action_reduce f state lookahead i j
  ;;

  let write_actions f state =
    Format.fprintf f "    match lookahead () with\n";
    List.iter (fun (l, m) -> write_action f state l m) state.s_action;
    let fold acc (term, _) = TermSet.union term acc in
    let expected = List.fold_left fold TermSet.empty state.s_action |> TermSet.elements in
    Format.fprintf f "    | _ -> fail [ %t ]\n" (fun f -> write_term_names f expected)
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
    List.iter (write_term_cons f) infos
  ;;

  let write_semantic_action_code f action =
    let n = List.length action.sa_args
    and code = action.sa_code in
    let write_part f l r =
      let len = r.Lexing.pos_cnum - l.Lexing.pos_cnum
      and ofs = l.pos_cnum - (fst action.sa_code.loc).pos_cnum in
      write_string f { data = String.sub (fst code.data) ofs len; loc = l, r }
    and get_impl = function
      | Ast.KwArg i ->
        (match List.nth_opt action.sa_args (i - 1) with
         | Some (Some a) -> a
         | Some None -> Printf.sprintf "_arg%d" i
         | None -> "()")
      | Ast.KwStartpos -> Printf.sprintf "_kw_startpos ~loc:_loc %d" n
      | Ast.KwEndpos -> Printf.sprintf "_kw_endpos ~loc:_loc %d" n
      | Ast.KwSymbolstartpos -> Printf.sprintf "_kw_symbolstartpos ~loc:_loc %d" n
      | Ast.KwStartofs -> Printf.sprintf "_kw_startofs ~loc:_loc %d" n
      | Ast.KwEndofs -> Printf.sprintf "_kw_endofs ~loc:_loc %d" n
      | Ast.KwSymbolstartofs -> Printf.sprintf "_kw_symbolstartofs ~loc:_loc %d" n
      | Ast.KwLoc -> Printf.sprintf "_kw_loc ~loc:_loc %d" n
      | Ast.KwSloc -> Printf.sprintf "_kw_sloc ~loc:_loc %d" n
    in
    let rec loop pos = function
      | [] -> write_part f pos (snd code.loc)
      | (kw, loc) :: kws ->
        write_part f pos (fst loc);
        let impl = get_impl kw in
        Format.fprintf f "(%s) " impl;
        loop (snd loc) kws
    in
    loop (fst action.sa_code.loc) (snd action.sa_code.data |> List.rev)
  ;;

  let write_semantic_action f id action =
    let item = List.nth (G.group action.sa_symbol).g_items action.sa_index in
    let iter i s = function
      | _ when symbol_has_value s = false -> ()
      | Some a -> Format.fprintf f " %s" a
      | None -> Format.fprintf f " _arg%d" (List.length action.sa_args - i)
    in
    write_semantic_action_id f action id;
    if S.locations then Format.fprintf f " ~loc:_loc";
    iteri2 iter (List.rev item.i_suffix) (List.rev action.sa_args);
    Format.fprintf f " () = %t" (fun f -> write_semantic_action_code f action)
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
      "%t%s%t%t =\n"
      (fun f -> write_state_id f id)
      (if S.locations then " ~loc" else "")
      (fun f -> write_arg_ids f (List.hd state.s_kernel).g_prefix)
      (fun f -> write_cont_ids f (fun _ -> true) state.s_kernel)
  ;;

  let write_state_body f state =
    let kn = List.length state.s_kernel in
    let gen_state_cont_def i group pre post =
      let fc f = write_cont_definition f state group (i + kn) in
      Format.fprintf f "    %s %t%s\n" pre fc post
    in
    letrec gen_state_cont_def state.s_closure;
    let group = List.hd state.s_kernel in
    if group.g_starting && (List.hd group.g_items).i_suffix = []
    then write_actions_starting f state
    else write_actions f state
  ;;

  let write_state f id state =
    write_state_sig f id state;
    write_state_body f state
  ;;

  let write_entry f symbol id =
    Format.fprintf
      f
      "let %s lexfun lexbuf =\n\
      \  States.setup lexfun lexbuf;\n\
      \  States.%t%s (fun x -> x)\n\
       ;;\n"
      (nterm_name symbol)
      (fun f -> write_state_id f id)
      (if S.locations then " ~loc:[]" else "")
  ;;

  let write f =
    let write_semantic_action f id a =
      Format.fprintf f "  let %t\n" (fun f -> write_semantic_action f id a)
    and write_state f _ (id, s) pre post =
      if S.comments then write_state_comment f s;
      Format.fprintf f "  %s %t%s" pre (fun f -> write_state f id s) post
    and write_entry f (nt, s) = write_entry f nt s
    and state_letrec = letrec ~post:"\n" ~post':"  ;;\n" in
    Format.fprintf
      f
      "%s\n\
       %t\n\n\
       %t\n\
       module Actions = struct\n\
       %s\n\
       %tend\n\n\
       module States = struct\n\
       %s\n\
       %tend\n\n\
       %t\n\
       %s"
      prelude
      (fun f -> List.iter (write_string f) A.automaton.a_header)
      (fun f -> write_term_type f G.symbols)
      action_lib
      (fun f -> IntMap.iter (write_semantic_action f) A.automaton.a_actions)
      state_lib
      (fun f -> IntMap.bindings A.automaton.a_states |> state_letrec (write_state f))
      (fun f -> List.iter (write_entry f) A.automaton.a_starting)
      epilogue
  ;;
end
