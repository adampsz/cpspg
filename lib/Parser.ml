[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

open Ast

let mknode ~loc data = { loc; data }

let plus, star, qmark =
  let loc = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym name = NTerm (mknode ~loc name) in
  sym "nonempty_list", sym "list", sym "option"
;;

type token =
  | TYPE of (string)
  | TID of (string)
  | STAR
  | SEMI
  | RPAREN
  | QMARK
  | PLUS
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
  let a1_decl ~loc:_loc code () = DeclCode (mknode ~loc:(_kw_loc ~loc:_loc 1) code)
  let a2_decl ~loc:_loc xs tp _arg1 () = DeclToken (tp, xs)
  let a3_tp ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a4_option ~loc:_loc () = None
  let a5_option ~loc:_loc x () = Some x
  let a6_tid ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a7_list ~loc:_loc () = []
  let a8_list ~loc:_loc xs x () = x :: xs
  let a9_decl ~loc:_loc xs tp _arg1 () = DeclStart (tp, xs)
  let a10_id ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a11_decl ~loc:_loc xs tp _arg1 () = DeclType (tp, xs)
  let a12_symbol ~loc:_loc name () = NTerm name
  let a13_symbol ~loc:_loc name () = Term name
  let a14_decl ~loc:_loc xs _arg1 () = DeclLeft xs
  let a15_decl ~loc:_loc xs _arg1 () = DeclRight xs
  let a16_decl ~loc:_loc xs _arg1 () = DeclNonassoc xs
  let a17_rule ~loc:_loc _arg5 prods _arg3 params id () = { id; params; prods }
  let a18_rule_parameters ~loc:_loc () = []
  let a19_rule_parameters ~loc:_loc _arg3 params _arg1 () = params
  let a20_rule_parameter_list ~loc:_loc () = []
  let a21_rule_parameter_list ~loc:_loc x () = [ x ]
  let a22_rule_parameter_list ~loc:_loc xs _arg2 x () = x :: xs
  let a23_rule_prods ~loc:_loc xs () = xs
  let a24_productions ~loc:_loc () = []
  let a25_productions ~loc:_loc xs x _arg1 () = x :: xs
  let a26_production ~loc:_loc action prec prod () = { prod; prec; action }
  let a27_producer ~loc:_loc actual _arg2 id () = { id = Some id; actual }
  let a28_actual ~loc:_loc _arg2 actual () = { symbol = plus; args = [ Arg actual ] }
  let a29_actual ~loc:_loc _arg2 actual () = { symbol = star; args = [ Arg actual ] }
  let a30_actual ~loc:_loc _arg2 actual () = { symbol = qmark; args = [ Arg actual ] }
  let a31_actual ~loc:_loc symbol () = { symbol; args = [] }
  let a32_actual ~loc:_loc _arg4 args _arg2 symbol () = { symbol; args }
  let a33_actual_args ~loc:_loc () = []
  let a34_actual_args ~loc:_loc x () = [ Arg x ]
  let a35_actual_args ~loc:_loc xs _arg2 x () = Arg x :: xs
  let a36_producer ~loc:_loc actual () = { id = None; actual }
  let a37_prec ~loc:_loc x _arg1 () = x
  let a38_code ~loc:_loc x () = mknode ~loc:(_kw_loc ~loc:_loc 1) x
  let a39_rule_prods ~loc:_loc xs x () = x :: xs
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
       grammar' → . list DSEP list EOF
       decl → . DTOKEN option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE tp list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 10
       DSTART -> 18
       DLEFT -> 23
       DRIGHT -> 25
       DNONASSOC -> 27
       decl -> 29
       list -> 31
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_decl ~loc x = state_29 ~loc x _c2_list
    and _c2_list ~loc x = state_31 ~loc x _c0_grammar_starting in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c1_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_18 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_23 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_25 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_27 ~loc _c1_decl
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
      and x = Actions.a1_decl ~loc a0_DCODE () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . tp 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       tp -> 4
       option -> 5
     ACTION:
       TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
       TYPE -> shift *)
  and state_2 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_4 ~loc x _c2_option
    and _c2_option ~loc x = state_5 ~loc x _c0_decl in
    match lookahead () with
    (* Reduce *)
    | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a4_option ~loc () in
      _c2_option ~loc x
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tp
    | _ -> fail [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tp → TYPE . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_3 ~loc a0_TYPE _c0_tp =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a3_tp ~loc a0_TYPE () in
      _c0_tp ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       option → tp . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_4 ~loc a0_tp _c0_option =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a5_option ~loc a0_tp () in
      _c0_option ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . tid list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 6
       tid -> 7
       list -> 9
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_5 ~loc a0_option _c0_decl =
    let rec _c1_tid ~loc x = state_7 ~loc x _c2_list
    and _c2_list ~loc x = state_9 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       tid → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_6 ~loc a0_TID _c0_tid =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a6_tid ~loc a0_TID () in
      _c0_tid ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       list → tid . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . tid list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 6
       tid -> 7
       list -> 8
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_7 ~loc a0_tid _c0_list =
    let rec _c1_tid ~loc x = state_7 ~loc x _c2_list
    and _c2_list ~loc x = state_8 ~loc x a0_tid _c0_list in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → tid list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_8 ~loc a0_list a1_tid _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_tid () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_9 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a2_decl ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE . tp list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
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
      state_3 ~loc x _c1_tp
    | _ -> fail [ "TYPE" ]

  (* ITEMS:
       decl → DTYPE tp . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 15
       list -> 17
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 4 1
       ID TID -> shift *)
  and state_11 ~loc a0_tp _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_15 ~loc x _c4_list
    and _c4_list ~loc x = state_17 ~loc x a0_tp _c0_decl in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       id → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COLON, COMMA, EQ, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COLON COMMA EQ PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_12 ~loc a0_ID _c0_id =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COLON | COMMA | EQ | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a10_id ~loc a0_ID () in
      _c0_id ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COLON"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → tid . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_13 ~loc a0_tid _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a13_symbol ~loc a0_tid () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → id . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_14 ~loc a0_id _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a12_symbol ~loc a0_id () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       list → symbol . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 15
       list -> 16
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 4 1
       ID TID -> shift *)
  and state_15 ~loc a0_symbol _c0_list =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_15 ~loc x _c4_list
    and _c4_list ~loc x = state_16 ~loc x a0_symbol _c0_list in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → symbol list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_16 ~loc a0_list a1_symbol _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_symbol () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE tp list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_17 ~loc a0_list a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a11_decl ~loc a0_list a1_tp () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tp → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . tp 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       tp -> 4
       option -> 19
     ACTION:
       ID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
       TYPE -> shift *)
  and state_18 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_4 ~loc x _c2_option
    and _c2_option ~loc x = state_19 ~loc x _c0_decl in
    match lookahead () with
    (* Reduce *)
    | ID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a4_option ~loc () in
      _c2_option ~loc x
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_tp
    | _ -> fail [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . id list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       id -> 20
       list -> 22
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_19 ~loc a0_option _c0_decl =
    let rec _c1_id ~loc x = state_20 ~loc x _c2_list
    and _c2_list ~loc x = state_22 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → id . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . id list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       id -> 20
       list -> 21
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_20 ~loc a0_id _c0_list =
    let rec _c1_id ~loc x = state_20 ~loc x _c2_list
    and _c2_list ~loc x = state_21 ~loc x a0_id _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → id list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_21 ~loc a0_list a1_id _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_id () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_22 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a9_decl ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 15
       list -> 24
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 4 1
       ID TID -> shift *)
  and state_23 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_15 ~loc x _c4_list
    and _c4_list ~loc x = state_24 ~loc x _c0_decl in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_24 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a14_decl ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 15
       list -> 26
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 4 1
       ID TID -> shift *)
  and state_25 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_15 ~loc x _c4_list
    and _c4_list ~loc x = state_26 ~loc x _c0_decl in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_26 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a15_decl ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       tid → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       id → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . id 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . tid 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 15
       list -> 28
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 4 1
       ID TID -> shift *)
  and state_27 ~loc _c0_decl =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_15 ~loc x _c4_list
    and _c4_list ~loc x = state_28 ~loc x _c0_decl in
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_28 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a16_decl ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl . list 		/ DSEP
       decl → . DTOKEN option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE tp list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 10
       DSTART -> 18
       DLEFT -> 23
       DRIGHT -> 25
       DNONASSOC -> 27
       decl -> 29
       list -> 30
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  and state_29 ~loc a0_decl _c0_list =
    let rec _c1_decl ~loc x = state_29 ~loc x _c2_list
    and _c2_list ~loc x = state_30 ~loc x a0_decl _c0_list in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c2_list ~loc x
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c1_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_18 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_23 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_25 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_27 ~loc _c1_decl
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl list . 		/ DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_30 ~loc a0_list a1_decl _c0_list =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_decl () in
      _c0_list ~loc x
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list . DSEP list EOF
     GOTO:
       DSEP -> 32
     ACTION:
       DSEP -> shift *)
  and state_31 ~loc a0_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_32 ~loc a0_list _c0_grammar_starting
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list DSEP . list EOF
       id → . ID 		/ COLON, LPAREN
       rule → . id rule_parameters COLON rule_prods SEMI 		/ ID, EOF
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       ID -> 12
       id -> 33
       rule -> 73
       list -> 75
     ACTION:
       ID -> shift
       EOF -> reduce 3 1 *)
  and state_32 ~loc a1_list _c0_grammar_starting =
    let rec _c1_id ~loc x = state_33 ~loc x _c2_rule
    and _c2_rule ~loc x = state_73 ~loc x _c3_list
    and _c3_list ~loc x = state_75 ~loc x a1_list _c0_grammar_starting in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rule → id . rule_parameters COLON rule_prods SEMI 		/ ID, EOF
       rule_parameters → . LPAREN rule_parameter_list RPAREN 		/ COLON
       rule_parameters → . 		/ COLON
     GOTO:
       LPAREN -> 34
       rule_parameters -> 40
     ACTION:
       LPAREN -> shift
       COLON -> reduce 1 1 *)
  and state_33 ~loc a0_id _c0_rule =
    let rec _c1_rule_parameters ~loc x = state_40 ~loc x a0_id _c0_rule in
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_34 ~loc _c1_rule_parameters
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a18_rule_parameters ~loc () in
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
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 35
       rule_parameter_list -> 38
     ACTION:
       RPAREN -> reduce 4 2
       ID TID -> shift *)
  and state_34 ~loc _c0_rule_parameters =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_35 ~loc x _c4_rule_parameter_list
    and _c4_rule_parameter_list ~loc x = state_38 ~loc x _c0_rule_parameters in
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a20_rule_parameter_list ~loc () in
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
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       rule_parameter_list → symbol . COMMA rule_parameter_list 		/ RPAREN
       rule_parameter_list → symbol . 		/ RPAREN
     GOTO:
       COMMA -> 36
     ACTION:
       RPAREN -> reduce 0 1
       COMMA -> shift *)
  and state_35 ~loc a0_symbol _c0_rule_parameter_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a21_rule_parameter_list ~loc a0_symbol () in
      _c0_rule_parameter_list ~loc x
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_36 ~loc a0_symbol _c0_rule_parameter_list
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
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 35
       rule_parameter_list -> 37
     ACTION:
       RPAREN -> reduce 4 2
       ID TID -> shift *)
  and state_36 ~loc a1_symbol _c0_rule_parameter_list =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_35 ~loc x _c4_rule_parameter_list
    and _c4_rule_parameter_list ~loc x = state_37 ~loc x a1_symbol _c0_rule_parameter_list in
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a20_rule_parameter_list ~loc () in
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
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       rule_parameter_list → symbol COMMA rule_parameter_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_37 ~loc a0_rule_parameter_list a2_symbol _c0_rule_parameter_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a22_rule_parameter_list ~loc a0_rule_parameter_list () a2_symbol () in
      _c0_rule_parameter_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       rule_parameters → LPAREN rule_parameter_list . RPAREN 		/ COLON
     GOTO:
       RPAREN -> 39
     ACTION:
       RPAREN -> shift *)
  and state_38 ~loc a0_rule_parameter_list _c0_rule_parameters =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_39 ~loc a0_rule_parameter_list _c0_rule_parameters
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       rule_parameters → LPAREN rule_parameter_list RPAREN . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_39 ~loc a1_rule_parameter_list _c0_rule_parameters =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a19_rule_parameters ~loc () a1_rule_parameter_list () () in
      _c0_rule_parameters ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → id rule_parameters . COLON rule_prods SEMI 		/ ID, EOF
     GOTO:
       COLON -> 41
     ACTION:
       COLON -> shift *)
  and state_40 ~loc a0_rule_parameters a1_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_41 ~loc a0_rule_parameters a1_id _c0_rule
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → id rule_parameters COLON . rule_prods SEMI 		/ ID, EOF
       tid → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, PLUS, QMARK, STAR, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       rule_prods → . production productions 		/ SEMI
       rule_prods → . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
       production → . list option code 		/ BAR, SEMI
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 6
       BAR -> 42
       tid -> 13
       id -> 43
       symbol -> 45
       rule_prods -> 68
       productions -> 70
       production -> 71
       producer -> 58
       actual -> 59
       list -> 61
     ACTION:
       CODE DPREC -> reduce 9 1
       SEMI -> reduce 5 1
       ID TID BAR -> shift *)
  and state_41 ~loc a1_rule_parameters a2_id _c0_rule =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_43 ~loc x _c3_symbol _c7_producer
    and _c3_symbol ~loc x = state_45 ~loc x _c8_actual
    and _c4_rule_prods ~loc x = state_68 ~loc x a1_rule_parameters a2_id _c0_rule
    and _c5_productions ~loc x = state_70 ~loc x _c4_rule_prods
    and _c6_production ~loc x = state_71 ~loc x _c4_rule_prods
    and _c7_producer ~loc x = state_58 ~loc x _c9_list
    and _c8_actual ~loc x = state_59 ~loc x _c7_producer _c8_actual
    and _c9_list ~loc x = state_61 ~loc x _c6_production in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c9_list ~loc x
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24_productions ~loc () in
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
      state_6 ~loc x _c1_tid
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_42 ~loc _c5_productions
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "BAR"; "SEMI" ]

  (* ITEMS:
       productions → BAR . production productions 		/ SEMI
       tid → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, PLUS, QMARK, STAR, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       production → . list option code 		/ BAR, SEMI
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 43
       symbol -> 45
       production -> 56
       producer -> 58
       actual -> 59
       list -> 61
     ACTION:
       CODE DPREC -> reduce 7 1
       ID TID -> shift *)
  and state_42 ~loc _c0_productions =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_43 ~loc x _c3_symbol _c5_producer
    and _c3_symbol ~loc x = state_45 ~loc x _c6_actual
    and _c4_production ~loc x = state_56 ~loc x _c0_productions
    and _c5_producer ~loc x = state_58 ~loc x _c7_list
    and _c6_actual ~loc x = state_59 ~loc x _c5_producer _c6_actual
    and _c7_list ~loc x = state_61 ~loc x _c4_production in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c7_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       symbol → id . 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       producer → id . EQ actual 		/ ID, TID, CODE, DPREC
     GOTO:
       EQ -> 44
     ACTION:
       ID TID CODE DPREC PLUS QMARK STAR LPAREN -> reduce 0 0
       EQ -> shift *)
  and state_43 ~loc a0_id _c0_symbol _c1_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | PLUS | QMARK | STAR | LPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a12_symbol ~loc a0_id () in
      _c0_symbol ~loc x
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_44 ~loc a0_id _c1_producer
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "EQ"; "PLUS"; "QMARK"; "STAR"; "LPAREN" ]

  (* ITEMS:
       producer → id EQ . actual 		/ ID, TID, CODE, DPREC
       tid → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 45
       actual -> 55
     ACTION:
       ID TID -> shift *)
  and state_44 ~loc a1_id _c0_producer =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_45 ~loc x _c4_actual
    and _c4_actual ~loc x = state_55 ~loc x a1_id _c0_producer _c4_actual in
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
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       actual → symbol . LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       actual → symbol . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       LPAREN -> 46
     ACTION:
       LPAREN -> shift
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 1 *)
  and state_45 ~loc a0_symbol _c0_actual =
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc a0_symbol _c0_actual
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a31_actual ~loc a0_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN . actual_args RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       tid → . TID 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       id → . ID 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . id 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . tid 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual PLUS 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual STAR 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual QMARK 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . symbol 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual_args → . actual COMMA actual_args 		/ RPAREN
       actual_args → . actual 		/ RPAREN
       actual_args → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 45
       actual -> 47
       actual_args -> 53
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 5 2 *)
  and state_46 ~loc a1_symbol _c0_actual =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_45 ~loc x _c4_actual
    and _c4_actual ~loc x = state_47 ~loc x _c4_actual _c5_actual_args
    and _c5_actual_args ~loc x = state_53 ~loc x a1_symbol _c0_actual in
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
      state_6 ~loc x _c1_tid
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a33_actual_args ~loc () in
      _c5_actual_args ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       actual → actual . PLUS 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → actual . STAR 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → actual . QMARK 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual_args → actual . COMMA actual_args 		/ RPAREN
       actual_args → actual . 		/ RPAREN
     GOTO:
       COMMA -> 48
       PLUS -> 50
       QMARK -> 51
       STAR -> 52
     ACTION:
       COMMA PLUS QMARK STAR -> shift
       RPAREN -> reduce 1 1 *)
  and state_47 ~loc a0_actual _c0_actual _c1_actual_args =
    match lookahead () with
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_48 ~loc a0_actual _c1_actual_args
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc a0_actual _c0_actual
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_51 ~loc a0_actual _c0_actual
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc a0_actual _c0_actual
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a34_actual_args ~loc a0_actual () in
      _c1_actual_args ~loc x
    | _ -> fail [ "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual_args → actual COMMA . actual_args 		/ RPAREN
       tid → . TID 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       id → . ID 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . id 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . tid 		/ COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       actual → . symbol LPAREN actual_args RPAREN 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual PLUS 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual STAR 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . actual QMARK 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . symbol 		/ COMMA, PLUS, QMARK, STAR, RPAREN
       actual_args → . actual COMMA actual_args 		/ RPAREN
       actual_args → . actual 		/ RPAREN
       actual_args → . 		/ RPAREN
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 45
       actual -> 47
       actual_args -> 49
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 5 2 *)
  and state_48 ~loc a1_actual _c0_actual_args =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_45 ~loc x _c4_actual
    and _c4_actual ~loc x = state_47 ~loc x _c4_actual _c5_actual_args
    and _c5_actual_args ~loc x = state_49 ~loc x a1_actual _c0_actual_args in
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
      state_6 ~loc x _c1_tid
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a33_actual_args ~loc () in
      _c5_actual_args ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       actual_args → actual COMMA actual_args . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_49 ~loc a0_actual_args a2_actual _c0_actual_args =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a35_actual_args ~loc a0_actual_args () a2_actual () in
      _c0_actual_args ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       actual → actual PLUS . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_50 ~loc a1_actual _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a28_actual ~loc () a1_actual () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → actual QMARK . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_51 ~loc a1_actual _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a30_actual ~loc () a1_actual () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → actual STAR . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_52 ~loc a1_actual _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a29_actual ~loc () a1_actual () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN actual_args . RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       RPAREN -> 54
     ACTION:
       RPAREN -> shift *)
  and state_53 ~loc a0_actual_args a2_symbol _c0_actual =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_54 ~loc a0_actual_args a2_symbol _c0_actual
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       actual → symbol LPAREN actual_args RPAREN . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_54 ~loc a1_actual_args a3_symbol _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 4
      and x = Actions.a32_actual ~loc () a1_actual_args () a3_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       producer → id EQ actual . 		/ ID, TID, CODE, DPREC
       actual → actual . PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → actual . STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → actual . QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       PLUS -> 50
       QMARK -> 51
       STAR -> 52
     ACTION:
       ID TID CODE DPREC -> reduce 0 0
       PLUS QMARK STAR -> shift *)
  and state_55 ~loc a0_actual a2_id _c0_producer _c1_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a27_producer ~loc a0_actual () a2_id () in
      _c0_producer ~loc x
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc a0_actual _c1_actual
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_51 ~loc a0_actual _c1_actual
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc a0_actual _c1_actual
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "PLUS"; "QMARK"; "STAR" ]

  (* ITEMS:
       productions → BAR production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 42
       productions -> 57
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_56 ~loc a0_production _c0_productions =
    let rec _c1_productions ~loc x = state_57 ~loc x a0_production _c0_productions in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_42 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       productions → BAR production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_57 ~loc a0_productions a1_production _c0_productions =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a25_productions ~loc a0_productions a1_production () () in
      _c0_productions ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       list → producer . list 		/ CODE, DPREC
       tid → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       id → . ID 		/ ID, TID, CODE, DPREC, EQ, PLUS, QMARK, STAR, LPAREN
       symbol → . id 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . tid 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       producer → . id EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . symbol LPAREN actual_args RPAREN 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . actual QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 43
       symbol -> 45
       producer -> 58
       actual -> 59
       list -> 60
     ACTION:
       CODE DPREC -> reduce 6 1
       ID TID -> shift *)
  and state_58 ~loc a0_producer _c0_list =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_43 ~loc x _c3_symbol _c4_producer
    and _c3_symbol ~loc x = state_45 ~loc x _c5_actual
    and _c4_producer ~loc x = state_58 ~loc x _c6_list
    and _c5_actual ~loc x = state_59 ~loc x _c4_producer _c5_actual
    and _c6_list ~loc x = state_60 ~loc x a0_producer _c0_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c6_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       producer → actual . 		/ ID, TID, CODE, DPREC
       actual → actual . PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → actual . STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → actual . QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       PLUS -> 50
       QMARK -> 51
       STAR -> 52
     ACTION:
       ID TID CODE DPREC -> reduce 0 0
       PLUS QMARK STAR -> shift *)
  and state_59 ~loc a0_actual _c0_producer _c1_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a36_producer ~loc a0_actual () in
      _c0_producer ~loc x
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc a0_actual _c1_actual
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_51 ~loc a0_actual _c1_actual
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc a0_actual _c1_actual
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "PLUS"; "QMARK"; "STAR" ]

  (* ITEMS:
       list → producer list . 		/ CODE, DPREC
     GOTO:
       
     ACTION:
       CODE DPREC -> reduce 0 0 *)
  and state_60 ~loc a0_list a1_producer _c0_list =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_producer () in
      _c0_list ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       production → list . option code 		/ BAR, SEMI
       prec → . DPREC symbol 		/ CODE
       option → . prec 		/ CODE
       option → . 		/ CODE
     GOTO:
       DPREC -> 62
       prec -> 64
       option -> 65
     ACTION:
       DPREC -> shift
       CODE -> reduce 2 1 *)
  and state_61 ~loc a0_list _c0_production =
    let rec _c1_prec ~loc x = state_64 ~loc x _c2_option
    and _c2_option ~loc x = state_65 ~loc x a0_list _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_62 ~loc _c1_prec
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a4_option ~loc () in
      _c2_option ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       prec → DPREC . symbol 		/ CODE
       tid → . TID 		/ CODE
       id → . ID 		/ CODE
       symbol → . id 		/ CODE
       symbol → . tid 		/ CODE
     GOTO:
       ID -> 12
       TID -> 6
       tid -> 13
       id -> 14
       symbol -> 63
     ACTION:
       ID TID -> shift *)
  and state_62 ~loc _c0_prec =
    let rec _c1_tid ~loc x = state_13 ~loc x _c3_symbol
    and _c2_id ~loc x = state_14 ~loc x _c3_symbol
    and _c3_symbol ~loc x = state_63 ~loc x _c0_prec in
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
      state_6 ~loc x _c1_tid
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       prec → DPREC symbol . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_63 ~loc a0_symbol _c0_prec =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a37_prec ~loc a0_symbol () () in
      _c0_prec ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       option → prec . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_64 ~loc a0_prec _c0_option =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a5_option ~loc a0_prec () in
      _c0_option ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       production → list option . code 		/ BAR, SEMI
       code → . CODE 		/ BAR, SEMI
     GOTO:
       CODE -> 66
       code -> 67
     ACTION:
       CODE -> shift *)
  and state_65 ~loc a0_option a1_list _c0_production =
    let rec _c1_code ~loc x = state_67 ~loc x a0_option a1_list _c0_production in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_66 ~loc x _c1_code
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       code → CODE . 		/ BAR, SEMI
     GOTO:
       
     ACTION:
       BAR SEMI -> reduce 0 0 *)
  and state_66 ~loc a0_CODE _c0_code =
    match lookahead () with
    (* Reduce *)
    | BAR | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a38_code ~loc a0_CODE () in
      _c0_code ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       production → list option code . 		/ BAR, SEMI
     GOTO:
       
     ACTION:
       BAR SEMI -> reduce 0 0 *)
  and state_67 ~loc a0_code a1_option a2_list _c0_production =
    match lookahead () with
    (* Reduce *)
    | BAR | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a26_production ~loc a0_code a1_option a2_list () in
      _c0_production ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       rule → id rule_parameters COLON rule_prods . SEMI 		/ ID, EOF
     GOTO:
       SEMI -> 69
     ACTION:
       SEMI -> shift *)
  and state_68 ~loc a0_rule_prods a2_rule_parameters a3_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_69 ~loc a0_rule_prods a2_rule_parameters a3_id _c0_rule
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule → id rule_parameters COLON rule_prods SEMI . 		/ ID, EOF
     GOTO:
       
     ACTION:
       ID EOF -> reduce 0 0 *)
  and state_69 ~loc a1_rule_prods a3_rule_parameters a4_id _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | EOF ->
      let loc = loc_reduce ~loc 5
      and x = Actions.a17_rule ~loc () a1_rule_prods () a3_rule_parameters a4_id () in
      _c0_rule ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       rule_prods → productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_70 ~loc a0_productions _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a23_rule_prods ~loc a0_productions () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule_prods → production . productions 		/ SEMI
       productions → . BAR production productions 		/ SEMI
       productions → . 		/ SEMI
     GOTO:
       BAR -> 42
       productions -> 72
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_71 ~loc a0_production _c0_rule_prods =
    let rec _c1_productions ~loc x = state_72 ~loc x a0_production _c0_rule_prods in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_42 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24_productions ~loc () in
      _c1_productions ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       rule_prods → production productions . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_72 ~loc a0_productions a1_production _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a39_rule_prods ~loc a0_productions a1_production () in
      _c0_rule_prods ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       list → rule . list 		/ EOF
       id → . ID 		/ COLON, LPAREN
       rule → . id rule_parameters COLON rule_prods SEMI 		/ ID, EOF
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       ID -> 12
       id -> 33
       rule -> 73
       list -> 74
     ACTION:
       ID -> shift
       EOF -> reduce 3 1 *)
  and state_73 ~loc a0_rule _c0_list =
    let rec _c1_id ~loc x = state_33 ~loc x _c2_rule
    and _c2_rule ~loc x = state_73 ~loc x _c3_list
    and _c3_list ~loc x = state_74 ~loc x a0_rule _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_12 ~loc x _c1_id
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a7_list ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "EOF" ]

  (* ITEMS:
       list → rule list . 		/ EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_74 ~loc a0_list a1_rule _c0_list =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a8_list ~loc a0_list a1_rule () in
      _c0_list ~loc x
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list . EOF
     GOTO:
       EOF -> 76
     ACTION:
       EOF -> shift *)
  and state_75 ~loc a0_list a2_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_76 ~loc a0_list a2_list _c0_grammar_starting
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list EOF .
     GOTO:
       
     ACTION:
        *)
  and state_76 ~loc a1_list a3_list _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a0_grammar ~loc () a1_list () a3_list () in
    _c0_grammar_starting x
  ;;
end

let grammar lexfun lexbuf =
  States.setup lexfun lexbuf;
  States.state_0 ~loc:[] (fun x -> x)
;;

let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens
