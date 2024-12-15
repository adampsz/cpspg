[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

open Ast

let mknode ~loc data = { loc; data }

type token =
  | TYPE of (string)
  | TID of (string)
  | SEMI
  | RPAREN
  | LPAREN
  | ID of (string)
  | EQ
  | EOF
  | DTYPE
  | DTOKEN
  | DSTART
  | DSEP
  | DRIGHT
  | DPREC
  | DNONASSOC
  | DLEFT
  | DCODE of (string)
  | COMMA
  | COLON
  | CODE of (code)
  | BAR

module Actions = struct
  let _kw_endpos ~loc _ =
    match loc with
    | l :: _ -> snd l
    | [] -> Lexing.dummy_pos
  ;;

  let _kw_startpos ~loc n =
    match List.nth_opt loc (n - 1) with
    | Some l -> fst l
    | None -> _kw_endpos ~loc n
  ;;

  let _kw_symbolstartpos ~loc:_ _ = failwith "unimplemented: $symbolstartpos"
  let _kw_startofs ~loc:_ _ = failwith "unimplemented: $startofs"
  let _kw_endofs ~loc:_ _ = failwith "unimplemented: $endofs"
  let _kw_symbolstartofs ~loc:_ _ = failwith "unimplemented: $symbolstartofs"
  let _kw_loc ~loc n = _kw_startpos ~loc n, _kw_endpos ~loc n
  let _kw_sloc ~loc:_ _ = failwith "unimplemented: $sloc"
  let a0_grammar ~loc:_loc _arg4 rules _arg2 decls () = { decls; rules }
  let a1_decls ~loc:_loc () = []
  let a2_decls ~loc:_loc xs x () = x :: xs
  let a3_decl ~loc:_loc code () = DeclCode (mknode ~loc:(_kw_loc ~loc:_loc 1) code)
  let a4_decl ~loc:_loc xs tp _arg1 () = DeclToken (Some tp, xs)
  let a5_tp ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a6_tids ~loc:_loc () = []
  let a7_tids ~loc:_loc xs x () = x :: xs
  let a8_tid ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a9_decl ~loc:_loc xs _arg1 () = DeclToken (None, xs)
  let a10_decl ~loc:_loc xs tp _arg1 () = DeclStart (Some tp, xs)
  let a11_ids ~loc:_loc () = []
  let a12_ids ~loc:_loc xs x () = x :: xs
  let a13_id ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a14_decl ~loc:_loc xs _arg1 () = DeclStart (None, xs)
  let a15_decl ~loc:_loc xs tp _arg1 () = DeclType (tp, xs)
  let a16_symbols ~loc:_loc () = []
  let a17_symbols ~loc:_loc xs x () = x :: xs
  let a18_symbol ~loc:_loc name () = NTerm name
  let a19_symbol ~loc:_loc name () = Term name
  let a20_decl ~loc:_loc xs _arg1 () = DeclLeft xs
  let a21_decl ~loc:_loc xs _arg1 () = DeclRight xs
  let a22_decl ~loc:_loc xs _arg1 () = DeclNonassoc xs
  let a23_rules ~loc:_loc () = []
  let a24_rules ~loc:_loc xs x () = x :: xs
  let a25_rule ~loc:_loc _arg5 prods _arg3 params id () = { id; params; prods }
  let a26_rule_parameters ~loc:_loc () = []
  let a27_rule_parameters ~loc:_loc _arg3 params _arg1 () = params
  let a28_rule_parameter_list ~loc:_loc () = []
  let a29_rule_parameter_list ~loc:_loc x () = [x]
  let a30_rule_parameter_list ~loc:_loc xs _arg2 x () = x :: xs
  let a31_rule_prods ~loc:_loc xs () = xs
  let a32_productions ~loc:_loc () = []
  let a33_productions ~loc:_loc xs x _arg1 () = x :: xs
  let a34_production ~loc:_loc action prec prod () = { prod; prec; action }
  let a35_producers ~loc:_loc () = []
  let a36_producers ~loc:_loc xs x () = x :: xs
  let a37_producer ~loc:_loc actual _arg2 id () = { id = Some id; actual }
  let a38_actual ~loc:_loc symbol () = { symbol; args = [] }
  let a39_actual ~loc:_loc _arg4 args _arg2 symbol () = { symbol; args }
  let a40_actual_args ~loc:_loc () = []
  let a41_actual_args ~loc:_loc x () = [Arg x]
  let a42_actual_args ~loc:_loc xs _arg2 x () = Arg x :: xs
  let a43_producer ~loc:_loc actual () = { id = None; actual }
  let a44_production_prec ~loc:_loc () = None
  let a45_production_prec ~loc:_loc x _arg1 () = Some x
  let a46_code ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a47_rule_prods ~loc:_loc xs x () = x :: xs
end

module States = struct
  let lexfun = ref (fun _ -> assert false)
  let lexbuf = ref (Lexing.from_string String.empty)
  let peeked = ref None
  let lexbuf_fallback_p = ref Lexing.dummy_pos
  let error_token = ref None
  let expected_tokens = ref []

  let setup lf lb =
    lexfun := lf;
    lexbuf := lb;
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p;
    error_token := None;
    expected_tokens := []
  ;;

  let shift () =
    let sym = Option.get !peeked in
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p;
    sym
  ;;

  let peek () =
    match !peeked with
    | Some p -> p
    | None ->
      let tok = !lexfun !lexbuf
      and loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in
      peeked := Some (tok, loc);
      tok, loc
  ;;

  let lookahead () = fst (peek ())

  let fail expected =
    let token, _ = peek () in
    error_token := Some token;
    expected_tokens := expected;
    raise Parsing.Parse_error
  ;;

  let loc_shift ~loc l = l :: loc

  let loc_reduce ~loc = function
    | 0 -> (!lexbuf_fallback_p, !lexbuf_fallback_p) :: loc
    | n ->
      let rec skip n xs = if n = 0 then xs else skip (n - 1) (List.tl xs) in
      let l = fst (List.nth loc (n - 1)), snd (List.hd loc) in
      l :: skip n loc
  ;;

  (* ITEMS:
       grammar' → . decls DSEP rules EOF
       decls → . decl decls 		/ DSEP
       decls → . 		/ DSEP
       decl → . DTOKEN tp tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART tp ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE tp symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTOKEN tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 10
       DSTART -> 18
       DLEFT -> 24
       DRIGHT -> 26
       DNONASSOC -> 28
       decls -> 30
       decl -> 72
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_decls ~loc x = state_30 ~loc x _c0_grammar_starting
    and _c2_decl ~loc x = state_72 ~loc x _c1_decls in
    match lookahead () with
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c2_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c2_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc _c2_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_18 ~loc _c2_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_24 ~loc _c2_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_26 ~loc _c2_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_28 ~loc _c2_decl
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a1_decls ~loc () in
      _c1_decls ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DCODE . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_1 ~loc a0_DCODE _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a3_decl ~loc a0_DCODE () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN . tp tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → DTOKEN . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       TYPE -> 4
       tp -> 5
       tids -> 9
       tid -> 7
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
       TID TYPE -> shift *)
  and state_2 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_5 ~loc x _c0_decl
    and _c2_tids ~loc x = state_9 ~loc x _c0_decl
    and _c3_tid ~loc x = state_7 ~loc x _c2_tids in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6_tids ~loc () in
      _c2_tids ~loc x
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c3_tid
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_4 ~loc x _c1_tp
    | _ -> fail [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tid → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA LPAREN RPAREN -> reduce 0 0 *)
  and state_3 ~loc a0_TID _c0_tid =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a8_tid ~loc a0_TID () in
      _c0_tid ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       tp → TYPE . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_4 ~loc a0_TYPE _c0_tp =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a5_tp ~loc a0_TYPE () in
      _c0_tp ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tp . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       tids -> 6
       tid -> 7
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_5 ~loc a0_tp _c0_decl =
    let rec _c1_tids ~loc x = state_6 ~loc x a0_tp _c0_decl
    and _c2_tid ~loc x = state_7 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c2_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6_tids ~loc () in
      _c1_tids ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tp tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_6 ~loc a0_tids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a4_decl ~loc a0_tids a1_tp () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tids → tid . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       tids -> 8
       tid -> 7
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_7 ~loc a0_tid _c0_tids =
    let rec _c1_tids ~loc x = state_8 ~loc x a0_tid _c0_tids
    and _c2_tid ~loc x = state_7 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c2_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6_tids ~loc () in
      _c1_tids ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tids → tid tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_8 ~loc a0_tids a1_tid _c0_tids =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7_tids ~loc a0_tids a1_tid () in
      _c0_tids ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_9 ~loc a0_tids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a9_decl ~loc a0_tids () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE . tp symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 4
       tp -> 11
     ACTION:
       TYPE -> shift *)
  and state_10 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_11 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_4 ~loc x _c1_tp
    | _ -> fail [ "TYPE" ]

  (* ITEMS:
       decl → DTYPE tp . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbols -> 15
       symbol -> 16
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 3 1 *)
  and state_11 ~loc a0_tp _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c4_symbol
    and _c2_id ~loc x = state_14 ~loc x _c4_symbol
    and _c3_symbols ~loc x = state_15 ~loc x a0_tp _c0_decl
    and _c4_symbol ~loc x = state_16 ~loc x _c3_symbols in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a16_symbols ~loc () in
      _c3_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       id → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COLON, COMMA, EQ, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COLON COMMA EQ LPAREN RPAREN -> reduce 0 0 *)
  and state_12 ~loc a0_ID _c0_id =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COLON | COMMA | EQ | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a13_id ~loc a0_ID () in
      _c0_id ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COLON"; "COMMA"; "EQ"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → tid . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA LPAREN RPAREN -> reduce 0 0 *)
  and state_13 ~loc a0_tid _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a19_symbol ~loc a0_tid () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → id . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA LPAREN RPAREN -> reduce 0 0 *)
  and state_14 ~loc a0_id _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a18_symbol ~loc a0_id () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       decl → DTYPE tp symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_15 ~loc a0_symbols a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a15_decl ~loc a0_symbols a1_tp () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbols → symbol . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbols -> 17
       symbol -> 16
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 3 1 *)
  and state_16 ~loc a0_symbol _c0_symbols =
    let rec _c1_tid ~loc x = state_13 ~loc x _c4_symbol
    and _c2_id ~loc x = state_14 ~loc x _c4_symbol
    and _c3_symbols ~loc x = state_17 ~loc x a0_symbol _c0_symbols
    and _c4_symbol ~loc x = state_16 ~loc x _c3_symbols in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a16_symbols ~loc () in
      _c3_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbols → symbol symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_17 ~loc a0_symbols a1_symbol _c0_symbols =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a17_symbols ~loc a0_symbols a1_symbol () in
      _c0_symbols ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART . tp ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → DSTART . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TYPE -> 4
       tp -> 19
       ids -> 23
       id -> 21
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
       ID TYPE -> shift *)
  and state_18 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_19 ~loc x _c0_decl
    and _c2_ids ~loc x = state_23 ~loc x _c0_decl
    and _c3_id ~loc x = state_21 ~loc x _c2_ids in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a11_ids ~loc () in
      _c2_ids ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_4 ~loc x _c1_tp
    | _ -> fail [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART tp . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       ids -> 20
       id -> 21
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_19 ~loc a0_tp _c0_decl =
    let rec _c1_ids ~loc x = state_20 ~loc x a0_tp _c0_decl
    and _c2_id ~loc x = state_21 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a11_ids ~loc () in
      _c1_ids ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART tp ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_20 ~loc a0_ids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a10_decl ~loc a0_ids a1_tp () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       ids → id . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       ids -> 22
       id -> 21
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_21 ~loc a0_id _c0_ids =
    let rec _c1_ids ~loc x = state_22 ~loc x a0_id _c0_ids
    and _c2_id ~loc x = state_21 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a11_ids ~loc () in
      _c1_ids ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       ids → id ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_22 ~loc a0_ids a1_id _c0_ids =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a12_ids ~loc a0_ids a1_id () in
      _c0_ids ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_23 ~loc a0_ids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a14_decl ~loc a0_ids () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbols -> 25
       symbol -> 16
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 3 1 *)
  and state_24 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c4_symbol
    and _c2_id ~loc x = state_14 ~loc x _c4_symbol
    and _c3_symbols ~loc x = state_25 ~loc x _c0_decl
    and _c4_symbol ~loc x = state_16 ~loc x _c3_symbols in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a16_symbols ~loc () in
      _c3_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_25 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a20_decl ~loc a0_symbols () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbols -> 27
       symbol -> 16
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 3 1 *)
  and state_26 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c4_symbol
    and _c2_id ~loc x = state_14 ~loc x _c4_symbol
    and _c3_symbols ~loc x = state_27 ~loc x _c0_decl
    and _c4_symbol ~loc x = state_16 ~loc x _c3_symbols in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a16_symbols ~loc () in
      _c3_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_27 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a21_decl ~loc a0_symbols () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbols -> 29
       symbol -> 16
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 3 1 *)
  and state_28 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c4_symbol
    and _c2_id ~loc x = state_14 ~loc x _c4_symbol
    and _c3_symbols ~loc x = state_29 ~loc x _c0_decl
    and _c4_symbol ~loc x = state_16 ~loc x _c3_symbols in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a16_symbols ~loc () in
      _c3_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_29 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a22_decl ~loc a0_symbols () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       grammar' → decls . DSEP rules EOF
     GOTO:
       DSEP -> 31
     ACTION:
       DSEP -> shift *)
  and state_30 ~loc a0_decls _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_31 ~loc a0_decls _c0_grammar_starting
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → decls DSEP . rules EOF
       id → . ID 		/ COLON, LPAREN
       rules → . rule rules 		/ EOF
       rules → . 		/ EOF
       rule → . id rule_parameters COLON rule_prods SEMI 		/ ID, EOF
     GOTO:
       ID -> 12
       id -> 32
       rules -> 68
       rule -> 70
     ACTION:
       ID -> shift
       EOF -> reduce 2 1 *)
  and state_31 ~loc a1_decls _c0_grammar_starting =
    let rec _c1_id ~loc x = state_32 ~loc x _c3_rule
    and _c2_rules ~loc x = state_68 ~loc x a1_decls _c0_grammar_starting
    and _c3_rule ~loc x = state_70 ~loc x _c2_rules in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a23_rules ~loc () in
      _c2_rules ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rule → id . rule_parameters COLON rule_prods SEMI 		/ ID, EOF
       rule_parameters → . LPAREN rule_parameter_list RPAREN 		/ COLON
       rule_parameters → . 		/ COLON
     GOTO:
       LPAREN -> 33
       rule_parameters -> 39
     ACTION:
       LPAREN -> shift
       COLON -> reduce 1 1 *)
  and state_32 ~loc a0_id _c0_rule =
    let rec _c1_rule_parameters ~loc x = state_39 ~loc x a0_id _c0_rule in
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_33 ~loc _c1_rule_parameters
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a26_rule_parameters ~loc () in
      _c1_rule_parameters ~loc x
    | _ -> fail [ "COLON"; "LPAREN" ]

  (* ITEMS:
       rule_parameters → LPAREN . rule_parameter_list RPAREN 		/ COLON
       tid → . TID 		/ COMMA, RPAREN
       id → . ID 		/ COMMA, RPAREN
       symbol → . id 		/ COMMA, RPAREN
       symbol → . tid 		/ COMMA, RPAREN
       rule_parameter_list → . symbol COMMA rule_parameter_list 		/ RPAREN
       rule_parameter_list → . symbol 		/ RPAREN
       rule_parameter_list → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 34
       rule_parameter_list -> 37
     ACTION:
       RPAREN -> reduce 4 2
       ID TID -> shift *)
  and state_33 ~loc _c0_rule_parameters =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_34 ~loc x _c4_rule_parameter_list
    and _c4_rule_parameter_list ~loc x = state_37 ~loc x _c0_rule_parameters in
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a28_rule_parameter_list ~loc () in
      _c4_rule_parameter_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       rule_parameter_list → symbol . COMMA rule_parameter_list 		/ RPAREN
       rule_parameter_list → symbol . 		/ RPAREN
     GOTO:
       COMMA -> 35
     ACTION:
       COMMA -> shift
       RPAREN -> reduce 0 1 *)
  and state_34 ~loc a0_symbol _c0_rule_parameter_list =
    match lookahead () with
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_35 ~loc a0_symbol _c0_rule_parameter_list
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a29_rule_parameter_list ~loc a0_symbol () in
      _c0_rule_parameter_list ~loc x
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       rule_parameter_list → symbol COMMA . rule_parameter_list 		/ RPAREN
       tid → . TID 		/ COMMA, RPAREN
       id → . ID 		/ COMMA, RPAREN
       symbol → . id 		/ COMMA, RPAREN
       symbol → . tid 		/ COMMA, RPAREN
       rule_parameter_list → . symbol COMMA rule_parameter_list 		/ RPAREN
       rule_parameter_list → . symbol 		/ RPAREN
       rule_parameter_list → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 34
       rule_parameter_list -> 36
     ACTION:
       RPAREN -> reduce 4 2
       ID TID -> shift *)
  and state_35 ~loc a1_symbol _c0_rule_parameter_list =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_34 ~loc x _c4_rule_parameter_list
    and _c4_rule_parameter_list ~loc x = state_36 ~loc x a1_symbol _c0_rule_parameter_list in
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a28_rule_parameter_list ~loc () in
      _c4_rule_parameter_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       rule_parameter_list → symbol COMMA rule_parameter_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_36 ~loc a0_rule_parameter_list a2_symbol _c0_rule_parameter_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a30_rule_parameter_list ~loc a0_rule_parameter_list () a2_symbol () in
      _c0_rule_parameter_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       rule_parameters → LPAREN rule_parameter_list . RPAREN 		/ COLON
     GOTO:
       RPAREN -> 38
     ACTION:
       RPAREN -> shift *)
  and state_37 ~loc a0_rule_parameter_list _c0_rule_parameters =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_38 ~loc a0_rule_parameter_list _c0_rule_parameters
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       rule_parameters → LPAREN rule_parameter_list RPAREN . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_38 ~loc a1_rule_parameter_list _c0_rule_parameters =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a27_rule_parameters ~loc () a1_rule_parameter_list () () in
      _c0_rule_parameters ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → id rule_parameters . COLON rule_prods SEMI 		/ ID, EOF
     GOTO:
       COLON -> 40
     ACTION:
       COLON -> shift *)
  and state_39 ~loc a0_rule_parameters a1_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_40 ~loc a0_rule_parameters a1_id _c0_rule
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → id rule_parameters COLON . rule_prods SEMI 		/ ID, EOF
       tid → . TID 		/ ID, TID, CODE, DPREC, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, LPAREN
       rule_prods → . production productions 		/ SEMI
       rule_prods → . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
       production → . producers production_prec code 		/ BAR, SEMI
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC
       actual → . symbol 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       BAR -> 41
       tid -> 13
       id -> 42
       symbol -> 44
       rule_prods -> 63
       productions -> 65
       production -> 66
       producers -> 54
       producer -> 60
       actual -> 62
     ACTION:
       CODE DPREC -> reduce 7 1
       SEMI -> reduce 5 1
       ID TID BAR -> shift *)
  and state_40 ~loc a1_rule_parameters a2_id _c0_rule =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_42 ~loc x _c3_symbol _c8_producer
    and _c3_symbol ~loc x = state_44 ~loc x _c9_actual
    and _c4_rule_prods ~loc x = state_63 ~loc x a1_rule_parameters a2_id _c0_rule
    and _c5_productions ~loc x = state_65 ~loc x _c4_rule_prods
    and _c6_production ~loc x = state_66 ~loc x _c4_rule_prods
    and _c7_producers ~loc x = state_54 ~loc x _c6_production
    and _c8_producer ~loc x = state_60 ~loc x _c7_producers
    and _c9_actual ~loc x = state_62 ~loc x _c8_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a35_producers ~loc () in
      _c7_producers ~loc x
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a32_productions ~loc () in
      _c5_productions ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_41 ~loc _c5_productions
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "BAR"; "SEMI" ]

  (* ITEMS:
       productions → BAR . production productions 		/ SEMI
       tid → . TID 		/ ID, TID, CODE, DPREC, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, LPAREN
       production → . producers production_prec code 		/ BAR, SEMI
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC
       actual → . symbol 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 42
       symbol -> 44
       production -> 52
       producers -> 54
       producer -> 60
       actual -> 62
     ACTION:
       CODE DPREC -> reduce 5 1
       ID TID -> shift *)
  and state_41 ~loc _c0_productions =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_42 ~loc x _c3_symbol _c6_producer
    and _c3_symbol ~loc x = state_44 ~loc x _c7_actual
    and _c4_production ~loc x = state_52 ~loc x _c0_productions
    and _c5_producers ~loc x = state_54 ~loc x _c4_production
    and _c6_producer ~loc x = state_60 ~loc x _c5_producers
    and _c7_actual ~loc x = state_62 ~loc x _c6_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a35_producers ~loc () in
      _c5_producers ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       symbol → id . 		/ ID, TID, CODE, DPREC, LPAREN
       producer → id . EQ actual 		/ ID, TID, CODE, DPREC
     GOTO:
       EQ -> 43
     ACTION:
       ID TID CODE DPREC LPAREN -> reduce 0 0
       EQ -> shift *)
  and state_42 ~loc a0_id _c0_symbol _c1_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | LPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a18_symbol ~loc a0_id () in
      _c0_symbol ~loc x
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_43 ~loc a0_id _c1_producer
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "EQ"; "LPAREN" ]

  (* ITEMS:
       producer → id EQ . actual 		/ ID, TID, CODE, DPREC
       tid → . TID 		/ ID, TID, CODE, DPREC, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, LPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC
       actual → . symbol 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 44
       actual -> 51
     ACTION:
       ID TID -> shift *)
  and state_43 ~loc a1_id _c0_producer =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_44 ~loc x _c4_actual
    and _c4_actual ~loc x = state_51 ~loc x a1_id _c0_producer in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       actual → symbol . LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, COMMA, RPAREN
       actual → symbol . 		/ ID, TID, CODE, DPREC, COMMA, RPAREN
     GOTO:
       LPAREN -> 45
     ACTION:
       LPAREN -> shift
       ID TID CODE DPREC COMMA RPAREN -> reduce 0 1 *)
  and state_44 ~loc a0_symbol _c0_actual =
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_45 ~loc a0_symbol _c0_actual
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a38_actual ~loc a0_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN . actual_args RPAREN 		/ ID, TID, CODE, DPREC, COMMA, RPAREN
       tid → . TID 		/ COMMA, LPAREN, RPAREN
       id → . ID 		/ COMMA, LPAREN, RPAREN
       symbol → . id 		/ COMMA, LPAREN, RPAREN
       symbol → . tid 		/ COMMA, LPAREN, RPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ COMMA, RPAREN
       actual → . symbol 		/ COMMA, RPAREN
       actual_args → . actual COMMA actual_args 		/ RPAREN
       actual_args → . actual 		/ RPAREN
       actual_args → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 44
       actual -> 46
       actual_args -> 49
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 5 2 *)
  and state_45 ~loc a1_symbol _c0_actual =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_44 ~loc x _c4_actual
    and _c4_actual ~loc x = state_46 ~loc x _c5_actual_args
    and _c5_actual_args ~loc x = state_49 ~loc x a1_symbol _c0_actual in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a40_actual_args ~loc () in
      _c5_actual_args ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       actual_args → actual . COMMA actual_args 		/ RPAREN
       actual_args → actual . 		/ RPAREN
     GOTO:
       COMMA -> 47
     ACTION:
       COMMA -> shift
       RPAREN -> reduce 0 1 *)
  and state_46 ~loc a0_actual _c0_actual_args =
    match lookahead () with
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_47 ~loc a0_actual _c0_actual_args
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a41_actual_args ~loc a0_actual () in
      _c0_actual_args ~loc x
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       actual_args → actual COMMA . actual_args 		/ RPAREN
       tid → . TID 		/ COMMA, LPAREN, RPAREN
       id → . ID 		/ COMMA, LPAREN, RPAREN
       symbol → . id 		/ COMMA, LPAREN, RPAREN
       symbol → . tid 		/ COMMA, LPAREN, RPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ COMMA, RPAREN
       actual → . symbol 		/ COMMA, RPAREN
       actual_args → . actual COMMA actual_args 		/ RPAREN
       actual_args → . actual 		/ RPAREN
       actual_args → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 44
       actual -> 46
       actual_args -> 48
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 5 2 *)
  and state_47 ~loc a1_actual _c0_actual_args =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_44 ~loc x _c4_actual
    and _c4_actual ~loc x = state_46 ~loc x _c5_actual_args
    and _c5_actual_args ~loc x = state_48 ~loc x a1_actual _c0_actual_args in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a40_actual_args ~loc () in
      _c5_actual_args ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       actual_args → actual COMMA actual_args . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_48 ~loc a0_actual_args a2_actual _c0_actual_args =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a42_actual_args ~loc a0_actual_args () a2_actual () in
      _c0_actual_args ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN actual_args . RPAREN 		/ ID, TID, CODE, DPREC, COMMA, RPAREN
     GOTO:
       RPAREN -> 50
     ACTION:
       RPAREN -> shift *)
  and state_49 ~loc a0_actual_args a2_symbol _c0_actual =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc a0_actual_args a2_symbol _c0_actual
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN actual_args RPAREN . 		/ ID, TID, CODE, DPREC, COMMA, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA RPAREN -> reduce 0 0 *)
  and state_50 ~loc a1_actual_args a3_symbol _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | RPAREN ->
      let loc = loc_reduce ~loc 4
      and x = Actions.a39_actual ~loc () a1_actual_args () a3_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "RPAREN" ]

  (* ITEMS:
       producer → id EQ actual . 		/ ID, TID, CODE, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_51 ~loc a0_actual a2_id _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a37_producer ~loc a0_actual () a2_id () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       productions → BAR production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 41
       productions -> 53
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_52 ~loc a0_production _c0_productions =
    let rec _c1_productions ~loc x = state_53 ~loc x a0_production _c0_productions in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_41 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a32_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       productions → BAR production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_53 ~loc a0_productions a1_production _c0_productions =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a33_productions ~loc a0_productions a1_production () () in
      _c0_productions ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       production → producers . production_prec code 		/ BAR, SEMI
       production_prec → . DPREC symbol 		/ CODE
       production_prec → . 		/ CODE
     GOTO:
       DPREC -> 55
       production_prec -> 57
     ACTION:
       DPREC -> shift
       CODE -> reduce 1 1 *)
  and state_54 ~loc a0_producers _c0_production =
    let rec _c1_production_prec ~loc x = state_57 ~loc x a0_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_55 ~loc _c1_production_prec
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a44_production_prec ~loc () in
      _c1_production_prec ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       production_prec → DPREC . symbol 		/ CODE
       tid → . TID 		/ CODE
       id → . ID 		/ CODE
       symbol → . id 		/ CODE
       symbol → . tid 		/ CODE
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 14
       symbol -> 56
     ACTION:
       ID TID -> shift *)
  and state_55 ~loc _c0_production_prec =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_56 ~loc x _c0_production_prec in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       production_prec → DPREC symbol . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_56 ~loc a0_symbol _c0_production_prec =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a45_production_prec ~loc a0_symbol () () in
      _c0_production_prec ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       production → producers production_prec . code 		/ BAR, SEMI
       code → . CODE 		/ BAR, SEMI
     GOTO:
       CODE -> 58
       code -> 59
     ACTION:
       CODE -> shift *)
  and state_57 ~loc a0_production_prec a1_producers _c0_production =
    let rec _c1_code ~loc x = state_59 ~loc x a0_production_prec a1_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc x _c1_code
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       code → CODE . 		/ BAR, SEMI
     GOTO:
       
     ACTION:
       BAR SEMI -> reduce 0 0 *)
  and state_58 ~loc a0_CODE _c0_code =
    match lookahead () with
    (* Reduce *)
    | BAR | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a46_code ~loc a0_CODE () in
      _c0_code ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       production → producers production_prec code . 		/ BAR, SEMI
     GOTO:
       
     ACTION:
       BAR SEMI -> reduce 0 0 *)
  and state_59 ~loc a0_code a1_production_prec a2_producers _c0_production =
    match lookahead () with
    (* Reduce *)
    | BAR | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a34_production ~loc a0_code a1_production_prec a2_producers () in
      _c0_production ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       producers → producer . producers 		/ CODE, DPREC
       tid → . TID 		/ ID, TID, CODE, DPREC, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, LPAREN
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC
       actual → . symbol 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       tid -> 13
       id -> 42
       symbol -> 44
       producers -> 61
       producer -> 60
       actual -> 62
     ACTION:
       CODE DPREC -> reduce 4 1
       ID TID -> shift *)
  and state_60 ~loc a0_producer _c0_producers =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_42 ~loc x _c3_symbol _c5_producer
    and _c3_symbol ~loc x = state_44 ~loc x _c6_actual
    and _c4_producers ~loc x = state_61 ~loc x a0_producer _c0_producers
    and _c5_producer ~loc x = state_60 ~loc x _c4_producers
    and _c6_actual ~loc x = state_62 ~loc x _c5_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a35_producers ~loc () in
      _c4_producers ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       producers → producer producers . 		/ CODE, DPREC
     GOTO:
       
     ACTION:
       CODE DPREC -> reduce 0 0 *)
  and state_61 ~loc a0_producers a1_producer _c0_producers =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a36_producers ~loc a0_producers a1_producer () in
      _c0_producers ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       producer → actual . 		/ ID, TID, CODE, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_62 ~loc a0_actual _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a43_producer ~loc a0_actual () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       rule → id rule_parameters COLON rule_prods . SEMI 		/ ID, EOF
     GOTO:
       SEMI -> 64
     ACTION:
       SEMI -> shift *)
  and state_63 ~loc a0_rule_prods a2_rule_parameters a3_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_64 ~loc a0_rule_prods a2_rule_parameters a3_id _c0_rule
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule → id rule_parameters COLON rule_prods SEMI . 		/ ID, EOF
     GOTO:
       
     ACTION:
       ID EOF -> reduce 0 0 *)
  and state_64 ~loc a1_rule_prods a3_rule_parameters a4_id _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | EOF ->
      let loc = loc_reduce ~loc 5
      and x = Actions.a25_rule ~loc () a1_rule_prods () a3_rule_parameters a4_id () in
      _c0_rule ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rule_prods → productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_65 ~loc a0_productions _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a31_rule_prods ~loc a0_productions () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule_prods → production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 41
       productions -> 67
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_66 ~loc a0_production _c0_rule_prods =
    let rec _c1_productions ~loc x = state_67 ~loc x a0_production _c0_rule_prods in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_41 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a32_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       rule_prods → production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_67 ~loc a0_productions a1_production _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a47_rule_prods ~loc a0_productions a1_production () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       grammar' → decls DSEP rules . EOF
     GOTO:
       EOF -> 69
     ACTION:
       EOF -> shift *)
  and state_68 ~loc a0_rules a2_decls _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_69 ~loc a0_rules a2_decls _c0_grammar_starting
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → decls DSEP rules EOF .
     GOTO:
       
     ACTION:
        *)
  and state_69 ~loc a1_rules a3_decls _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a0_grammar ~loc () a1_rules () a3_decls () in
    _c0_grammar_starting x

  (* ITEMS:
       rules → rule . rules 		/ EOF
       id → . ID 		/ COLON, LPAREN
       rules → . rule rules 		/ EOF
       rules → . 		/ EOF
       rule → . id rule_parameters COLON rule_prods SEMI 		/ ID, EOF
     GOTO:
       ID -> 12
       id -> 32
       rules -> 71
       rule -> 70
     ACTION:
       ID -> shift
       EOF -> reduce 2 1 *)
  and state_70 ~loc a0_rule _c0_rules =
    let rec _c1_id ~loc x = state_32 ~loc x _c3_rule
    and _c2_rules ~loc x = state_71 ~loc x a0_rule _c0_rules
    and _c3_rule ~loc x = state_70 ~loc x _c2_rules in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a23_rules ~loc () in
      _c2_rules ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rules → rule rules . 		/ EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_71 ~loc a0_rules a1_rule _c0_rules =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a24_rules ~loc a0_rules a1_rule () in
      _c0_rules ~loc x
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       decls → decl . decls 		/ DSEP
       decls → . decl decls 		/ DSEP
       decls → . 		/ DSEP
       decl → . DTOKEN tp tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART tp ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE tp symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTOKEN tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 10
       DSTART -> 18
       DLEFT -> 24
       DRIGHT -> 26
       DNONASSOC -> 28
       decls -> 73
       decl -> 72
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  and state_72 ~loc a0_decl _c0_decls =
    let rec _c1_decls ~loc x = state_73 ~loc x a0_decl _c0_decls
    and _c2_decl ~loc x = state_72 ~loc x _c1_decls in
    match lookahead () with
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c2_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c2_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc _c2_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_18 ~loc _c2_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_24 ~loc _c2_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_26 ~loc _c2_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_28 ~loc _c2_decl
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a1_decls ~loc () in
      _c1_decls ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decls → decl decls . 		/ DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_73 ~loc a0_decls a1_decl _c0_decls =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a2_decls ~loc a0_decls a1_decl () in
      _c0_decls ~loc x
    | _ -> fail [ "DSEP" ]
  ;;
end

let grammar lexfun lexbuf =
  States.setup lexfun lexbuf;
  States.state_0 ~loc:[] (fun x -> x)
;;

let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens
