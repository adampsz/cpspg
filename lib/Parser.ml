[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

open Ast

let mknode ~loc data = { loc; data }

type token =
  | TYPE of (string)
  | TID of (string)
  | SEMI
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
  let a0_grammar ~loc:_loc rules decls () = { decls; rules }
  let a1_decls ~loc:_loc xs x () = x :: xs
  let a2_decls ~loc:_loc () = []
  let a3_decl ~loc:_loc xs tp () = DeclToken (Some tp, xs)
  let a4_decl ~loc:_loc xs tp () = DeclStart (Some tp, xs)
  let a5_decl ~loc:_loc xs tp () = DeclType (tp, xs)
  let a6_decl ~loc:_loc xs () = DeclToken (None, xs)
  let a7_decl ~loc:_loc xs () = DeclStart (None, xs)
  let a8_decl ~loc:_loc xs () = DeclLeft xs
  let a9_decl ~loc:_loc xs () = DeclRight xs
  let a10_decl ~loc:_loc xs () = DeclNonassoc xs
  let a11_decl ~loc:_loc code () = DeclCode (mknode ~loc:(_kw_loc ~loc:_loc 1) code)
  let a12_rules ~loc:_loc xs x () = x :: xs
  let a13_rules ~loc:_loc () = []
  let a14_rule ~loc:_loc prods id () = { id; prods }
  let a15_rule_prods ~loc:_loc xs x () = x :: xs
  let a16_rule_prods ~loc:_loc xs () = xs
  let a17_productions ~loc:_loc xs x () = x :: xs
  let a18_productions ~loc:_loc () = []
  let a19_production ~loc:_loc action prec prod () = { prod; prec; action }
  let a20_production_prec ~loc:_loc x () = Some x
  let a21_production_prec ~loc:_loc () = None
  let a22_producers ~loc:_loc xs x () = x :: xs
  let a23_producers ~loc:_loc () = []
  let a24_producer ~loc:_loc actual id () = { id = Some id; actual }
  let a25_producer ~loc:_loc actual () = { id = None; actual }
  let a26_ids ~loc:_loc xs x () = x :: xs
  let a27_ids ~loc:_loc () = []
  let a28_tids ~loc:_loc xs x () = x :: xs
  let a29_tids ~loc:_loc () = []
  let a30_symbols ~loc:_loc xs x () = x :: xs
  let a31_symbols ~loc:_loc () = []
  let a32_symbol ~loc:_loc name () = NTerm name
  let a33_symbol ~loc:_loc name () = Term name
  let a34_id ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a35_tid ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a36_tp ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a37_code ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
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
       decl -> 58
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_decls ~loc x = state_30 ~loc x _c0_grammar_starting
    and _c2_decl ~loc x = state_58 ~loc x _c1_decls in
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
      and x = Actions.a2_decls ~loc () in
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
      and x = Actions.a11_decl ~loc a0_DCODE () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN . tp tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → DTOKEN . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       TYPE -> 4
       tids -> 5
       tid -> 6
       tp -> 8
     ACTION:
       TID TYPE -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_2 ~loc _c0_decl =
    let rec _c1_tids ~loc x = state_5 ~loc x _c0_decl
    and _c2_tid ~loc x = state_6 ~loc x _c1_tids
    and _c3_tp ~loc x = state_8 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c2_tid
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_4 ~loc x _c3_tp
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a29_tids ~loc () in
      _c1_tids ~loc x
    | _ -> fail [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tid → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DPREC, DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_3 ~loc a0_TID _c0_tid =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a35_tid ~loc a0_TID () in
      _c0_tid ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DPREC"; "DSEP" ]

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
      and x = Actions.a36_tp ~loc a0_TYPE () in
      _c0_tp ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_5 ~loc a0_tids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a6_decl ~loc a0_tids () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tids → tid . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       tids -> 7
       tid -> 6
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_6 ~loc a0_tid _c0_tids =
    let rec _c1_tids ~loc x = state_7 ~loc x a0_tid _c0_tids
    and _c2_tid ~loc x = state_6 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c2_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a29_tids ~loc () in
      _c1_tids ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tids → tid tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_7 ~loc a0_tids a1_tid _c0_tids =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a28_tids ~loc a0_tids a1_tid () in
      _c0_tids ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tp . tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . tid tids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 3
       tids -> 9
       tid -> 6
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_8 ~loc a0_tp _c0_decl =
    let rec _c1_tids ~loc x = state_9 ~loc x a0_tp _c0_decl
    and _c2_tid ~loc x = state_6 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c2_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a29_tids ~loc () in
      _c1_tids ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN tp tids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_9 ~loc a0_tids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a3_decl ~loc a0_tids a1_tp () in
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
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       symbols -> 13
       symbol -> 14
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_11 ~loc a0_tp _c0_decl =
    let rec _c1_symbols ~loc x = state_13 ~loc x a0_tp _c0_decl
    and _c2_symbol ~loc x = state_14 ~loc x _c1_symbols
    and _c3_id ~loc x = state_16 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_17 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c4_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a31_symbols ~loc () in
      _c1_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       id → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DPREC, DSEP, COLON, EQ
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP COLON EQ -> reduce 0 0 *)
  and state_12 ~loc a0_ID _c0_id =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP | COLON | EQ ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a34_id ~loc a0_ID () in
      _c0_id ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DPREC"; "DSEP"; "COLON"; "EQ" ]

  (* ITEMS:
       decl → DTYPE tp symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_13 ~loc a0_symbols a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a5_decl ~loc a0_symbols a1_tp () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbols → symbol . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       symbols -> 15
       symbol -> 14
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_14 ~loc a0_symbol _c0_symbols =
    let rec _c1_symbols ~loc x = state_15 ~loc x a0_symbol _c0_symbols
    and _c2_symbol ~loc x = state_14 ~loc x _c1_symbols
    and _c3_id ~loc x = state_16 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_17 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c4_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a31_symbols ~loc () in
      _c1_symbols ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbols → symbol symbols . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_15 ~loc a0_symbols a1_symbol _c0_symbols =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a30_symbols ~loc a0_symbols a1_symbol () in
      _c0_symbols ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbol → id . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DPREC, DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_16 ~loc a0_id _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a32_symbol ~loc a0_id () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DPREC"; "DSEP" ]

  (* ITEMS:
       symbol → tid . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DPREC, DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_17 ~loc a0_tid _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a33_symbol ~loc a0_tid () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DPREC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART . tp ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → DSTART . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TYPE -> 4
       ids -> 19
       id -> 20
       tp -> 22
     ACTION:
       ID TYPE -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_18 ~loc _c0_decl =
    let rec _c1_ids ~loc x = state_19 ~loc x _c0_decl
    and _c2_id ~loc x = state_20 ~loc x _c1_ids
    and _c3_tp ~loc x = state_22 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_4 ~loc x _c3_tp
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a27_ids ~loc () in
      _c1_ids ~loc x
    | _ -> fail [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_19 ~loc a0_ids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7_decl ~loc a0_ids () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       ids → id . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       ids -> 21
       id -> 20
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_20 ~loc a0_id _c0_ids =
    let rec _c1_ids ~loc x = state_21 ~loc x a0_id _c0_ids
    and _c2_id ~loc x = state_20 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a27_ids ~loc () in
      _c1_ids ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       ids → id ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_21 ~loc a0_ids a1_id _c0_ids =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a26_ids ~loc a0_ids a1_id () in
      _c0_ids ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART tp . ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . id ids 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ids → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       ids -> 23
       id -> 20
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_22 ~loc a0_tp _c0_decl =
    let rec _c1_ids ~loc x = state_23 ~loc x a0_tp _c0_decl
    and _c2_id ~loc x = state_20 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a27_ids ~loc () in
      _c1_ids ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART tp ids . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_23 ~loc a0_ids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a4_decl ~loc a0_ids a1_tp () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       symbols -> 25
       symbol -> 14
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_24 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_25 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_14 ~loc x _c1_symbols
    and _c3_id ~loc x = state_16 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_17 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c4_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a31_symbols ~loc () in
      _c1_symbols ~loc x
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
      and x = Actions.a8_decl ~loc a0_symbols () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       symbols -> 27
       symbol -> 14
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_26 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_27 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_14 ~loc x _c1_symbols
    and _c3_id ~loc x = state_16 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_17 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c4_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a31_symbols ~loc () in
      _c1_symbols ~loc x
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
      and x = Actions.a9_decl ~loc a0_symbols () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC . symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . symbol symbols 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbols → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 3
       symbols -> 29
       symbol -> 14
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_28 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_29 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_14 ~loc x _c1_symbols
    and _c3_id ~loc x = state_16 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_17 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c4_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a31_symbols ~loc () in
      _c1_symbols ~loc x
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
      and x = Actions.a10_decl ~loc a0_symbols () in
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
       rules → . rule rules 		/ EOF
       rules → . 		/ EOF
       rule → . id COLON rule_prods SEMI 		/ ID, EOF
       id → . ID 		/ COLON
     GOTO:
       ID -> 12
       rules -> 32
       rule -> 34
       id -> 36
     ACTION:
       ID -> shift
       EOF -> reduce 1 1 *)
  and state_31 ~loc a1_decls _c0_grammar_starting =
    let rec _c1_rules ~loc x = state_32 ~loc x a1_decls _c0_grammar_starting
    and _c2_rule ~loc x = state_34 ~loc x _c1_rules
    and _c3_id ~loc x = state_36 ~loc x _c2_rule in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a13_rules ~loc () in
      _c1_rules ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       grammar' → decls DSEP rules . EOF
     GOTO:
       EOF -> 33
     ACTION:
       EOF -> shift *)
  and state_32 ~loc a0_rules a2_decls _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_33 ~loc a0_rules a2_decls _c0_grammar_starting
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → decls DSEP rules EOF .
     GOTO:
       
     ACTION:
        *)
  and state_33 ~loc a1_rules a3_decls _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a0_grammar ~loc a1_rules a3_decls () in
    _c0_grammar_starting x

  (* ITEMS:
       rules → rule . rules 		/ EOF
       rules → . rule rules 		/ EOF
       rules → . 		/ EOF
       rule → . id COLON rule_prods SEMI 		/ ID, EOF
       id → . ID 		/ COLON
     GOTO:
       ID -> 12
       rules -> 35
       rule -> 34
       id -> 36
     ACTION:
       ID -> shift
       EOF -> reduce 1 1 *)
  and state_34 ~loc a0_rule _c0_rules =
    let rec _c1_rules ~loc x = state_35 ~loc x a0_rule _c0_rules
    and _c2_rule ~loc x = state_34 ~loc x _c1_rules
    and _c3_id ~loc x = state_36 ~loc x _c2_rule in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c3_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a13_rules ~loc () in
      _c1_rules ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rules → rule rules . 		/ EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_35 ~loc a0_rules a1_rule _c0_rules =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a12_rules ~loc a0_rules a1_rule () in
      _c0_rules ~loc x
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       rule → id . COLON rule_prods SEMI 		/ ID, EOF
     GOTO:
       COLON -> 37
     ACTION:
       COLON -> shift *)
  and state_36 ~loc a0_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_37 ~loc a0_id _c0_rule
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → id COLON . rule_prods SEMI 		/ ID, EOF
       rule_prods → . production productions 		/ SEMI
       rule_prods → . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
       production → . producers production_prec code 		/ SEMI, BAR
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ symbol 		/ ID, TID, CODE, DPREC
       producer → . symbol 		/ ID, TID, CODE, DPREC
       symbol → . id 		/ ID, TID, CODE, DPREC
       symbol → . tid 		/ ID, TID, CODE, DPREC
       id → . ID 		/ ID, TID, CODE, DPREC, EQ
       tid → . TID 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       BAR -> 38
       rule_prods -> 53
       productions -> 55
       production -> 56
       producers -> 41
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 17
     ACTION:
       CODE DPREC -> reduce 4 1
       ID TID BAR -> shift
       SEMI -> reduce 2 1 *)
  and state_37 ~loc a1_id _c0_rule =
    let rec _c1_rule_prods ~loc x = state_53 ~loc x a1_id _c0_rule
    and _c2_productions ~loc x = state_55 ~loc x _c1_rule_prods
    and _c3_production ~loc x = state_56 ~loc x _c1_rule_prods
    and _c4_producers ~loc x = state_41 ~loc x _c3_production
    and _c5_producer ~loc x = state_47 ~loc x _c4_producers
    and _c6_symbol ~loc x = state_49 ~loc x _c5_producer
    and _c7_id ~loc x = state_50 ~loc x _c5_producer _c6_symbol
    and _c8_tid ~loc x = state_17 ~loc x _c6_symbol in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a23_producers ~loc () in
      _c4_producers ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c7_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c8_tid
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_38 ~loc _c2_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a18_productions ~loc () in
      _c2_productions ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "SEMI"; "BAR" ]

  (* ITEMS:
       productions → BAR . production productions 		/ SEMI
       production → . producers production_prec code 		/ SEMI, BAR
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ symbol 		/ ID, TID, CODE, DPREC
       producer → . symbol 		/ ID, TID, CODE, DPREC
       symbol → . id 		/ ID, TID, CODE, DPREC
       symbol → . tid 		/ ID, TID, CODE, DPREC
       id → . ID 		/ ID, TID, CODE, DPREC, EQ
       tid → . TID 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       production -> 39
       producers -> 41
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 17
     ACTION:
       ID TID -> shift
       CODE DPREC -> reduce 2 1 *)
  and state_38 ~loc _c0_productions =
    let rec _c1_production ~loc x = state_39 ~loc x _c0_productions
    and _c2_producers ~loc x = state_41 ~loc x _c1_production
    and _c3_producer ~loc x = state_47 ~loc x _c2_producers
    and _c4_symbol ~loc x = state_49 ~loc x _c3_producer
    and _c5_id ~loc x = state_50 ~loc x _c3_producer _c4_symbol
    and _c6_tid ~loc x = state_17 ~loc x _c4_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c5_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c6_tid
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a23_producers ~loc () in
      _c2_producers ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       productions → BAR production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 38
       productions -> 40
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_39 ~loc a0_production _c0_productions =
    let rec _c1_productions ~loc x = state_40 ~loc x a0_production _c0_productions in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_38 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a18_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "SEMI"; "BAR" ]

  (* ITEMS:
       productions → BAR production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_40 ~loc a0_productions a1_production _c0_productions =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a17_productions ~loc a0_productions a1_production () in
      _c0_productions ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       production → producers . production_prec code 		/ SEMI, BAR
       production_prec → . DPREC symbol 		/ CODE
       production_prec → . 		/ CODE
     GOTO:
       DPREC -> 42
       production_prec -> 44
     ACTION:
       DPREC -> shift
       CODE -> reduce 1 1 *)
  and state_41 ~loc a0_producers _c0_production =
    let rec _c1_production_prec ~loc x = state_44 ~loc x a0_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_42 ~loc _c1_production_prec
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a21_production_prec ~loc () in
      _c1_production_prec ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       production_prec → DPREC . symbol 		/ CODE
       symbol → . id 		/ CODE
       symbol → . tid 		/ CODE
       id → . ID 		/ CODE
       tid → . TID 		/ CODE
     GOTO:
       ID -> 12
       TID -> 3
       symbol -> 43
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift *)
  and state_42 ~loc _c0_production_prec =
    let rec _c1_symbol ~loc x = state_43 ~loc x _c0_production_prec
    and _c2_id ~loc x = state_16 ~loc x _c1_symbol
    and _c3_tid ~loc x = state_17 ~loc x _c1_symbol in
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
      state_3 ~loc x _c3_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       production_prec → DPREC symbol . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_43 ~loc a0_symbol _c0_production_prec =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a20_production_prec ~loc a0_symbol () in
      _c0_production_prec ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       production → producers production_prec . code 		/ SEMI, BAR
       code → . CODE 		/ SEMI, BAR
     GOTO:
       CODE -> 45
       code -> 46
     ACTION:
       CODE -> shift *)
  and state_44 ~loc a0_production_prec a1_producers _c0_production =
    let rec _c1_code ~loc x = state_46 ~loc x a0_production_prec a1_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_45 ~loc x _c1_code
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       code → CODE . 		/ SEMI, BAR
     GOTO:
       
     ACTION:
       SEMI BAR -> reduce 0 0 *)
  and state_45 ~loc a0_CODE _c0_code =
    match lookahead () with
    (* Reduce *)
    | SEMI | BAR ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a37_code ~loc a0_CODE () in
      _c0_code ~loc x
    | _ -> fail [ "SEMI"; "BAR" ]

  (* ITEMS:
       production → producers production_prec code . 		/ SEMI, BAR
     GOTO:
       
     ACTION:
       SEMI BAR -> reduce 0 0 *)
  and state_46 ~loc a0_code a1_production_prec a2_producers _c0_production =
    match lookahead () with
    (* Reduce *)
    | SEMI | BAR ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a19_production ~loc a0_code a1_production_prec a2_producers () in
      _c0_production ~loc x
    | _ -> fail [ "SEMI"; "BAR" ]

  (* ITEMS:
       producers → producer . producers 		/ CODE, DPREC
       producers → . producer producers 		/ CODE, DPREC
       producers → . 		/ CODE, DPREC
       producer → . id EQ symbol 		/ ID, TID, CODE, DPREC
       producer → . symbol 		/ ID, TID, CODE, DPREC
       symbol → . id 		/ ID, TID, CODE, DPREC
       symbol → . tid 		/ ID, TID, CODE, DPREC
       id → . ID 		/ ID, TID, CODE, DPREC, EQ
       tid → . TID 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       producers -> 48
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 17
     ACTION:
       ID TID -> shift
       CODE DPREC -> reduce 1 1 *)
  and state_47 ~loc a0_producer _c0_producers =
    let rec _c1_producers ~loc x = state_48 ~loc x a0_producer _c0_producers
    and _c2_producer ~loc x = state_47 ~loc x _c1_producers
    and _c3_symbol ~loc x = state_49 ~loc x _c2_producer
    and _c4_id ~loc x = state_50 ~loc x _c2_producer _c3_symbol
    and _c5_tid ~loc x = state_17 ~loc x _c3_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c4_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c5_tid
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a23_producers ~loc () in
      _c1_producers ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       producers → producer producers . 		/ CODE, DPREC
     GOTO:
       
     ACTION:
       CODE DPREC -> reduce 0 0 *)
  and state_48 ~loc a0_producers a1_producer _c0_producers =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a22_producers ~loc a0_producers a1_producer () in
      _c0_producers ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       producer → symbol . 		/ ID, TID, CODE, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_49 ~loc a0_symbol _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a25_producer ~loc a0_symbol () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       producer → id . EQ symbol 		/ ID, TID, CODE, DPREC
       symbol → id . 		/ ID, TID, CODE, DPREC
     GOTO:
       EQ -> 51
     ACTION:
       EQ -> shift
       ID TID CODE DPREC -> reduce 1 0 *)
  and state_50 ~loc a0_id _c0_producer _c1_symbol =
    match lookahead () with
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_51 ~loc a0_id _c0_producer
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a32_symbol ~loc a0_id () in
      _c1_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "EQ" ]

  (* ITEMS:
       producer → id EQ . symbol 		/ ID, TID, CODE, DPREC
       symbol → . id 		/ ID, TID, CODE, DPREC
       symbol → . tid 		/ ID, TID, CODE, DPREC
       id → . ID 		/ ID, TID, CODE, DPREC
       tid → . TID 		/ ID, TID, CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 3
       symbol -> 52
       id -> 16
       tid -> 17
     ACTION:
       ID TID -> shift *)
  and state_51 ~loc a1_id _c0_producer =
    let rec _c1_symbol ~loc x = state_52 ~loc x a1_id _c0_producer
    and _c2_id ~loc x = state_16 ~loc x _c1_symbol
    and _c3_tid ~loc x = state_17 ~loc x _c1_symbol in
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
      state_3 ~loc x _c3_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       producer → id EQ symbol . 		/ ID, TID, CODE, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_52 ~loc a0_symbol a2_id _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a24_producer ~loc a0_symbol a2_id () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       rule → id COLON rule_prods . SEMI 		/ ID, EOF
     GOTO:
       SEMI -> 54
     ACTION:
       SEMI -> shift *)
  and state_53 ~loc a0_rule_prods a2_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_54 ~loc a0_rule_prods a2_id _c0_rule
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule → id COLON rule_prods SEMI . 		/ ID, EOF
     GOTO:
       
     ACTION:
       ID EOF -> reduce 0 0 *)
  and state_54 ~loc a1_rule_prods a3_id _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | EOF ->
      let loc = loc_reduce ~loc 4
      and x = Actions.a14_rule ~loc a1_rule_prods a3_id () in
      _c0_rule ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rule_prods → productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_55 ~loc a0_productions _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a16_rule_prods ~loc a0_productions () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule_prods → production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 38
       productions -> 57
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_56 ~loc a0_production _c0_rule_prods =
    let rec _c1_productions ~loc x = state_57 ~loc x a0_production _c0_rule_prods in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_38 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a18_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "SEMI"; "BAR" ]

  (* ITEMS:
       rule_prods → production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_57 ~loc a0_productions a1_production _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a15_rule_prods ~loc a0_productions a1_production () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

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
       decls -> 59
       decl -> 58
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  and state_58 ~loc a0_decl _c0_decls =
    let rec _c1_decls ~loc x = state_59 ~loc x a0_decl _c0_decls
    and _c2_decl ~loc x = state_58 ~loc x _c1_decls in
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
      and x = Actions.a2_decls ~loc () in
      _c1_decls ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decls → decl decls . 		/ DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_59 ~loc a0_decls a1_decl _c0_decls =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a1_decls ~loc a0_decls a1_decl () in
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
