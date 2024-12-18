[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

open Ast

let plus, star, qmark =
  let loc = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym data = NTerm { loc; data } in
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
  | DINLINE
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
  let a0 ~loc:_loc data () = (DeclCode { loc=(_kw_loc ~loc:_loc 1) ; data })
  let a1 ~loc:_loc data () = ({ loc =(_kw_loc ~loc:_loc 1) ; data })
  let a2 ~loc:_loc _arg1 () = ((_arg1) )
  let a3 ~loc:_loc () = (None)
  let a4 ~loc:_loc x () = (Some x)
  let a5 ~loc:_loc _arg1 () = ((_arg1) )
  let a6 ~loc:_loc () = ([])
  let a7 ~loc:_loc xs x () = (x :: xs)
  let a8 ~loc:_loc xs tp _arg1 () = (DeclToken (tp, xs))
  let a9 ~loc:_loc _arg1 () = ((_arg1) )
  let a10 ~loc:_loc xs tp _arg1 () = (DeclStart (tp, xs))
  let a11 ~loc:_loc name () = (NTerm name)
  let a12 ~loc:_loc name () = (Term name)
  let a13 ~loc:_loc xs tp _arg1 () = (DeclType (tp, xs))
  let a14 ~loc:_loc xs _arg1 () = (DeclLeft xs)
  let a15 ~loc:_loc xs _arg1 () = (DeclRight xs)
  let a16 ~loc:_loc xs _arg1 () = (DeclNonassoc xs)
  let a17 ~loc:_loc () = (false)
  let a18 ~loc:_loc _arg1 () = (true)
  let a19 ~loc:_loc x () = (x)
  let a20 ~loc:_loc x () = ([ x ])
  let a21 ~loc:_loc xs _arg2 x () = (x :: xs)
  let a22 ~loc:_loc () = ([])
  let a23 ~loc:_loc x () = (x)
  let a24 ~loc:_loc xs () = (xs)
  let a25 ~loc:_loc _arg3 params _arg1 () = (params)
  let a26 ~loc:_loc _arg2 x () = (x)
  let a27 ~loc:_loc () = (None)
  let a28 ~loc:_loc x () = (Some x)
  let a29 ~loc:_loc _arg1 () = (plus)
  let a30 ~loc:_loc _arg1 () = (star)
  let a31 ~loc:_loc _arg1 () = (qmark)
  let a32 ~loc:_loc symbol actual () = ({ symbol; args = [ Arg actual ] })
  let a33 ~loc:_loc x () = (Arg x)
  let a34 ~loc:_loc _arg1 () = ((_arg1) )
  let a35 ~loc:_loc action prod () = (ArgInline { prod; action })
  let a36 ~loc:_loc _arg3 args _arg1 () = (args)
  let a37 ~loc:_loc args symbol () = ({ symbol; args })
  let a38 ~loc:_loc actual id () = ({ id; actual })
  let a39 ~loc:_loc x _arg1 () = (x)
  let a40 ~loc:_loc action prec prod () = ({ prod; prec; action })
  let a41 ~loc:_loc _arg7 prods _arg5 _arg4 params id inline () = ({ id; inline; params; prods })
  let a42 ~loc:_loc _arg4 rules _arg2 decls () = ({ decls; rules })
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
       decl → . DTYPE TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 8
       DSTART -> 15
       DLEFT -> 20
       DRIGHT -> 22
       DNONASSOC -> 24
       decl -> 26
       list -> 28
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_decl ~loc x = state_26 ~loc x _c2_list
    and _c2_list ~loc x = state_28 ~loc x _c0_grammar_starting in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
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
      state_8 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_15 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_20 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_24 ~loc _c1_decl
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
      and x = Actions.a0 ~loc a0_DCODE () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       option -> 4
     ACTION:
       TYPE -> shift
       TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_2 ~loc _c0_decl =
    let rec _c1_option ~loc x = state_4 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_option
    (* Reduce *)
    | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       option → TYPE . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_3 ~loc a0_TYPE _c0_option =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a4 ~loc (Actions.a2 ~loc (Actions.a1 ~loc a0_TYPE ()) ()) () in
      _c0_option ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 5
       list -> 7
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_4 ~loc a0_option _c0_decl =
    let rec _c1_list ~loc x = state_7 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_5 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → TID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 5
       list -> 6
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_5 ~loc a0_TID _c0_list =
    let rec _c1_list ~loc x = state_6 ~loc x a0_TID _c0_list in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_5 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → TID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_6 ~loc a0_list a1_TID _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list (Actions.a5 ~loc (Actions.a1 ~loc a1_TID ()) ()) () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_7 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a8 ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE . TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 9
     ACTION:
       TYPE -> shift *)
  and state_8 ~loc _c0_decl =
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_9 ~loc x _c0_decl
    | _ -> fail [ "TYPE" ]

  (* ITEMS:
       decl → DTYPE TYPE . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 14
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_9 ~loc a0_TYPE _c0_decl =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_14 ~loc x a0_TYPE _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbol → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_10 ~loc a0_ID _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a11 ~loc (Actions.a9 ~loc (Actions.a1 ~loc a0_ID ()) ()) () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_11 ~loc a0_TID _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a12 ~loc (Actions.a5 ~loc (Actions.a1 ~loc a0_TID ()) ()) () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       list → symbol . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 13
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_12 ~loc a0_symbol _c0_list =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_13 ~loc x a0_symbol _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → symbol list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_13 ~loc a0_list a1_symbol _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_symbol () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE TYPE list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_14 ~loc a0_list a1_TYPE _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a13 ~loc a0_list (Actions.a2 ~loc (Actions.a1 ~loc a1_TYPE ()) ()) () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       option -> 16
     ACTION:
       TYPE -> shift
       ID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_15 ~loc _c0_decl =
    let rec _c1_option ~loc x = state_16 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_option
    (* Reduce *)
    | ID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 17
       list -> 19
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_16 ~loc a0_option _c0_decl =
    let rec _c1_list ~loc x = state_19 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_17 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → ID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 17
       list -> 18
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_17 ~loc a0_ID _c0_list =
    let rec _c1_list ~loc x = state_18 ~loc x a0_ID _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_17 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → ID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_18 ~loc a0_list a1_ID _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list (Actions.a9 ~loc (Actions.a1 ~loc a1_ID ()) ()) () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_19 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a10 ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 21
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_20 ~loc _c0_decl =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_21 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_21 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a14 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 23
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_22 ~loc _c0_decl =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_23 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_23 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a15 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 25
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_24 ~loc _c0_decl =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_25 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_25 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a16 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl . list 		/ DSEP
       decl → . DTOKEN option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 8
       DSTART -> 15
       DLEFT -> 20
       DRIGHT -> 22
       DNONASSOC -> 24
       decl -> 26
       list -> 27
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  and state_26 ~loc a0_decl _c0_list =
    let rec _c1_decl ~loc x = state_26 ~loc x _c2_list
    and _c2_list ~loc x = state_27 ~loc x a0_decl _c0_list in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
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
      state_8 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_15 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_20 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_24 ~loc _c1_decl
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl list . 		/ DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_27 ~loc a0_list a1_decl _c0_list =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_decl () in
      _c0_list ~loc x
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list . DSEP list EOF
     GOTO:
       DSEP -> 29
     ACTION:
       DSEP -> shift *)
  and state_28 ~loc a0_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_29 ~loc a0_list _c0_grammar_starting
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list DSEP . list EOF
       rule → . boption ID loption COLON option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
       boption → . DINLINE 		/ ID
       boption → . 		/ ID
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       DINLINE -> 30
       rule -> 31
       boption -> 32
       list -> 80
     ACTION:
       ID -> reduce 2 1
       DINLINE -> shift
       EOF -> reduce 3 1 *)
  and state_29 ~loc a1_list _c0_grammar_starting =
    let rec _c1_rule ~loc x = state_31 ~loc x _c3_list
    and _c2_boption ~loc x = state_32 ~loc x _c1_rule
    and _c3_list ~loc x = state_80 ~loc x a1_list _c0_grammar_starting in
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a17 ~loc () in
      _c2_boption ~loc x
    (* Shift *)
    | DINLINE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_30 ~loc _c2_boption
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       boption → DINLINE . 		/ ID
     GOTO:
       
     ACTION:
       ID -> reduce 0 0 *)
  and state_30 ~loc _c0_boption =
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a18 ~loc () () in
      _c0_boption ~loc x
    | _ -> fail [ "ID" ]

  (* ITEMS:
       list → rule . list 		/ EOF
       rule → . boption ID loption COLON option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
       boption → . DINLINE 		/ ID
       boption → . 		/ ID
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       DINLINE -> 30
       rule -> 31
       boption -> 32
       list -> 79
     ACTION:
       ID -> reduce 2 1
       DINLINE -> shift
       EOF -> reduce 3 1 *)
  and state_31 ~loc a0_rule _c0_list =
    let rec _c1_rule ~loc x = state_31 ~loc x _c3_list
    and _c2_boption ~loc x = state_32 ~loc x _c1_rule
    and _c3_list ~loc x = state_79 ~loc x a0_rule _c0_list in
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a17 ~loc () in
      _c2_boption ~loc x
    (* Shift *)
    | DINLINE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_30 ~loc _c2_boption
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       rule → boption . ID loption COLON option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
     GOTO:
       ID -> 33
     ACTION:
       ID -> shift *)
  and state_32 ~loc a0_boption _c0_rule =
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_33 ~loc x a0_boption _c0_rule
    | _ -> fail [ "ID" ]

  (* ITEMS:
       rule → boption ID . loption COLON option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
       parameters → . LPAREN loption RPAREN 		/ COLON
       loption → . parameters 		/ COLON
       loption → . 		/ COLON
     GOTO:
       LPAREN -> 34
       parameters -> 41
       loption -> 42
     ACTION:
       COLON -> reduce 2 1
       LPAREN -> shift *)
  and state_33 ~loc a0_ID a1_boption _c0_rule =
    let rec _c1_parameters ~loc x = state_41 ~loc x _c2_loption
    and _c2_loption ~loc x = state_42 ~loc x a0_ID a1_boption _c0_rule in
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a22 ~loc () in
      _c2_loption ~loc x
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_34 ~loc _c1_parameters
    | _ -> fail [ "COLON"; "LPAREN" ]

  (* ITEMS:
       parameters → LPAREN . loption RPAREN 		/ COLON
       symbol → . ID 		/ COMMA, RPAREN
       symbol → . TID 		/ COMMA, RPAREN
       separated_nonempty_list → . symbol COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . symbol 		/ RPAREN
       loption → . separated_nonempty_list 		/ RPAREN
       loption → . 		/ RPAREN
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 35
       separated_nonempty_list -> 38
       loption -> 39
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 3 1 *)
  and state_34 ~loc _c0_parameters =
    let rec _c1_symbol ~loc x = state_35 ~loc x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~loc x = state_38 ~loc x _c3_loption
    and _c3_loption ~loc x = state_39 ~loc x _c0_parameters in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a22 ~loc () in
      _c3_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → symbol . COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → symbol . 		/ RPAREN
     GOTO:
       COMMA -> 36
     ACTION:
       RPAREN -> reduce 0 1
       COMMA -> shift *)
  and state_35 ~loc a0_symbol _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a20 ~loc (Actions.a19 ~loc a0_symbol ()) () in
      _c0_separated_nonempty_list ~loc x
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_36 ~loc a0_symbol _c0_separated_nonempty_list
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → symbol COMMA . separated_nonempty_list 		/ RPAREN
       symbol → . ID 		/ COMMA, RPAREN
       symbol → . TID 		/ COMMA, RPAREN
       separated_nonempty_list → . symbol COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . symbol 		/ RPAREN
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 35
       separated_nonempty_list -> 37
     ACTION:
       ID TID -> shift *)
  and state_36 ~loc a1_symbol _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_35 ~loc x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~loc x = state_37 ~loc x a1_symbol _c0_separated_nonempty_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       separated_nonempty_list → symbol COMMA separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_37 ~loc a0_separated_nonempty_list a2_symbol _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a21 ~loc a0_separated_nonempty_list () (Actions.a19 ~loc a2_symbol ()) () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       loption → separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_38 ~loc a0_separated_nonempty_list _c0_loption =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a23 ~loc a0_separated_nonempty_list () in
      _c0_loption ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       parameters → LPAREN loption . RPAREN 		/ COLON
     GOTO:
       RPAREN -> 40
     ACTION:
       RPAREN -> shift *)
  and state_39 ~loc a0_loption _c0_parameters =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_40 ~loc a0_loption _c0_parameters
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       parameters → LPAREN loption RPAREN . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_40 ~loc a1_loption _c0_parameters =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a25 ~loc () (Actions.a24 ~loc a1_loption ()) () () in
      _c0_parameters ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       loption → parameters . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_41 ~loc a0_parameters _c0_loption =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a23 ~loc a0_parameters () in
      _c0_loption ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → boption ID loption . COLON option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
     GOTO:
       COLON -> 43
     ACTION:
       COLON -> shift *)
  and state_42 ~loc a0_loption a1_ID a2_boption _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_43 ~loc a0_loption a1_ID a2_boption _c0_rule
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → boption ID loption COLON . option separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
       option → . BAR 		/ ID, TID, CODE, DPREC
       option → . 		/ ID, TID, CODE, DPREC
     GOTO:
       BAR -> 44
       option -> 45
     ACTION:
       BAR -> shift
       ID TID CODE DPREC -> reduce 1 1 *)
  and state_43 ~loc a1_loption a2_ID a3_boption _c0_rule =
    let rec _c1_option ~loc x = state_45 ~loc x a1_loption a2_ID a3_boption _c0_rule in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_44 ~loc _c1_option
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "BAR" ]

  (* ITEMS:
       option → BAR . 		/ ID, TID, CODE, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_44 ~loc _c0_option =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a4 ~loc () () in
      _c0_option ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       rule → boption ID loption COLON option . separated_nonempty_list SEMI 		/ ID, DINLINE, EOF
       symbol → . ID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       production → . list option CODE 		/ BAR, SEMI
       producer → . ID EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
       separated_nonempty_list → . production BAR separated_nonempty_list 		/ SEMI
       separated_nonempty_list → . production 		/ SEMI
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       production -> 69
       producer -> 50
       actual -> 51
       list -> 71
       separated_nonempty_list -> 77
     ACTION:
       CODE DPREC -> reduce 5 1
       ID TID -> shift *)
  and state_45 ~loc a0_option a2_loption a3_ID a4_boption _c0_rule =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c4_actual
    and _c2_production ~loc x = state_69 ~loc x _c6_separated_nonempty_list
    and _c3_producer ~loc x = state_50 ~loc x _c5_list
    and _c4_actual ~loc x = state_51 ~loc x _c3_producer _c4_actual
    and _c5_list ~loc x = state_71 ~loc x _c2_production
    and _c6_separated_nonempty_list ~loc x = state_77 ~loc x a0_option a2_loption a3_ID a4_boption _c0_rule in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       symbol → ID . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       producer → ID . EQ actual 		/ ID, TID, CODE, DPREC
     GOTO:
       EQ -> 47
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR LPAREN RPAREN -> reduce 0 0
       EQ -> shift *)
  and state_46 ~loc a0_ID _c0_symbol _c1_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a11 ~loc (Actions.a9 ~loc (Actions.a1 ~loc a0_ID ()) ()) () in
      _c0_symbol ~loc x
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_47 ~loc a0_ID _c1_producer
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       producer → ID EQ . actual 		/ ID, TID, CODE, DPREC
       symbol → . ID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       actual → . actual shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 48
       actual -> 68
     ACTION:
       ID TID -> shift *)
  and state_47 ~loc a1_ID _c0_producer =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c2_actual
    and _c2_actual ~loc x = state_68 ~loc x a1_ID _c0_producer _c2_actual in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       actual → symbol . loption 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       args → . LPAREN separated_nonempty_list RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       loption → . args 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       loption → . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       LPAREN -> 49
       args -> 66
       loption -> 67
     ACTION:
       LPAREN -> shift
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 2 1 *)
  and state_48 ~loc a0_symbol _c0_actual =
    let rec _c1_args ~loc x = state_66 ~loc x _c2_loption
    and _c2_loption ~loc x = state_67 ~loc x a0_symbol _c0_actual in
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_49 ~loc _c1_args
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a22 ~loc () in
      _c2_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       args → LPAREN . separated_nonempty_list RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
       symbol → . ID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . TID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       producer → . ID EQ actual 		/ ID, TID, CODE
       producer → . actual 		/ ID, TID, CODE
       actual → . actual shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . symbol loption 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       list → . producer list 		/ CODE
       list → . 		/ CODE
       separated_nonempty_list → . list CODE COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . actual COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . list CODE 		/ RPAREN
       separated_nonempty_list → . actual 		/ RPAREN
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       producer -> 50
       actual -> 57
       list -> 59
       separated_nonempty_list -> 64
     ACTION:
       CODE -> reduce 4 1
       ID TID -> shift *)
  and state_49 ~loc _c0_args =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c3_actual
    and _c2_producer ~loc x = state_50 ~loc x _c4_list
    and _c3_actual ~loc x = state_57 ~loc x _c2_producer _c3_actual _c5_separated_nonempty_list
    and _c4_list ~loc x = state_59 ~loc x _c5_separated_nonempty_list
    and _c5_separated_nonempty_list ~loc x = state_64 ~loc x _c0_args in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE" ]

  (* ITEMS:
       list → producer . list 		/ CODE, DPREC
       symbol → . ID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       producer → . ID EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       producer -> 50
       actual -> 51
       list -> 56
     ACTION:
       CODE DPREC -> reduce 4 1
       ID TID -> shift *)
  and state_50 ~loc a0_producer _c0_list =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c3_actual
    and _c2_producer ~loc x = state_50 ~loc x _c4_list
    and _c3_actual ~loc x = state_51 ~loc x _c2_producer _c3_actual
    and _c4_list ~loc x = state_56 ~loc x a0_producer _c0_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       producer → actual . 		/ ID, TID, CODE, DPREC
       actual → actual . shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       PLUS -> 52
       QMARK -> 53
       STAR -> 54
       shorthand -> 55
     ACTION:
       ID TID CODE DPREC -> reduce 0 0
       PLUS QMARK STAR -> shift *)
  and state_51 ~loc a0_actual _c0_producer _c1_actual =
    let rec _c2_shorthand ~loc x = state_55 ~loc x a0_actual _c1_actual in
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a38 ~loc a0_actual (Actions.a27 ~loc ()) () in
      _c0_producer ~loc x
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_53 ~loc _c2_shorthand
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_54 ~loc _c2_shorthand
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "PLUS"; "QMARK"; "STAR" ]

  (* ITEMS:
       shorthand → PLUS . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_52 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a29 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       shorthand → QMARK . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_53 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a31 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       shorthand → STAR . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_54 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a30 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → actual shorthand . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_55 ~loc a0_shorthand a1_actual _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a32 ~loc a0_shorthand a1_actual () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       list → producer list . 		/ CODE, DPREC
     GOTO:
       
     ACTION:
       CODE DPREC -> reduce 0 0 *)
  and state_56 ~loc a0_list a1_producer _c0_list =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_producer () in
      _c0_list ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       producer → actual . 		/ ID, TID, CODE
       actual → actual . shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       separated_nonempty_list → actual . COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → actual . 		/ RPAREN
       shorthand → . PLUS 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       shorthand → . STAR 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       shorthand → . QMARK 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       COMMA -> 58
       PLUS -> 52
       QMARK -> 53
       STAR -> 54
       shorthand -> 55
     ACTION:
       ID TID CODE -> reduce 0 0
       RPAREN -> reduce 2 1
       COMMA PLUS QMARK STAR -> shift *)
  and state_57 ~loc a0_actual _c0_producer _c1_actual _c2_separated_nonempty_list =
    let rec _c3_shorthand ~loc x = state_55 ~loc x a0_actual _c1_actual in
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a38 ~loc a0_actual (Actions.a27 ~loc ()) () in
      _c0_producer ~loc x
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a20 ~loc (Actions.a33 ~loc a0_actual ()) () in
      _c2_separated_nonempty_list ~loc x
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc a0_actual _c2_separated_nonempty_list
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc _c3_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_53 ~loc _c3_shorthand
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_54 ~loc _c3_shorthand
    | _ -> fail [ "ID"; "TID"; "CODE"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → actual COMMA . separated_nonempty_list 		/ RPAREN
       symbol → . ID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . TID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       producer → . ID EQ actual 		/ ID, TID, CODE
       producer → . actual 		/ ID, TID, CODE
       actual → . actual shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . symbol loption 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       list → . producer list 		/ CODE
       list → . 		/ CODE
       separated_nonempty_list → . list CODE COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . actual COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . list CODE 		/ RPAREN
       separated_nonempty_list → . actual 		/ RPAREN
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       producer -> 50
       actual -> 57
       list -> 59
       separated_nonempty_list -> 63
     ACTION:
       CODE -> reduce 4 1
       ID TID -> shift *)
  and state_58 ~loc a1_actual _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c3_actual
    and _c2_producer ~loc x = state_50 ~loc x _c4_list
    and _c3_actual ~loc x = state_57 ~loc x _c2_producer _c3_actual _c5_separated_nonempty_list
    and _c4_list ~loc x = state_59 ~loc x _c5_separated_nonempty_list
    and _c5_separated_nonempty_list ~loc x = state_63 ~loc x a1_actual _c0_separated_nonempty_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE" ]

  (* ITEMS:
       separated_nonempty_list → list . CODE COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → list . CODE 		/ RPAREN
     GOTO:
       CODE -> 60
     ACTION:
       CODE -> shift *)
  and state_59 ~loc a0_list _c0_separated_nonempty_list =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_60 ~loc x a0_list _c0_separated_nonempty_list
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       separated_nonempty_list → list CODE . COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → list CODE . 		/ RPAREN
     GOTO:
       COMMA -> 61
     ACTION:
       RPAREN -> reduce 0 1
       COMMA -> shift *)
  and state_60 ~loc a0_CODE a1_list _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a20 ~loc (Actions.a35 ~loc (Actions.a34 ~loc (Actions.a1 ~loc a0_CODE ()) ()) a1_list ()) () in
      _c0_separated_nonempty_list ~loc x
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_61 ~loc a0_CODE a1_list _c0_separated_nonempty_list
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → list CODE COMMA . separated_nonempty_list 		/ RPAREN
       symbol → . ID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       symbol → . TID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, LPAREN, RPAREN
       producer → . ID EQ actual 		/ ID, TID, CODE
       producer → . actual 		/ ID, TID, CODE
       actual → . actual shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       actual → . symbol loption 		/ ID, TID, CODE, COMMA, PLUS, QMARK, STAR, RPAREN
       list → . producer list 		/ CODE
       list → . 		/ CODE
       separated_nonempty_list → . list CODE COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . actual COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . list CODE 		/ RPAREN
       separated_nonempty_list → . actual 		/ RPAREN
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       producer -> 50
       actual -> 57
       list -> 59
       separated_nonempty_list -> 62
     ACTION:
       CODE -> reduce 4 1
       ID TID -> shift *)
  and state_61 ~loc a1_CODE a2_list _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c3_actual
    and _c2_producer ~loc x = state_50 ~loc x _c4_list
    and _c3_actual ~loc x = state_57 ~loc x _c2_producer _c3_actual _c5_separated_nonempty_list
    and _c4_list ~loc x = state_59 ~loc x _c5_separated_nonempty_list
    and _c5_separated_nonempty_list ~loc x = state_62 ~loc x a1_CODE a2_list _c0_separated_nonempty_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE" ]

  (* ITEMS:
       separated_nonempty_list → list CODE COMMA separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_62 ~loc a0_separated_nonempty_list a2_CODE a3_list _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 4
      and x = Actions.a21 ~loc a0_separated_nonempty_list () (Actions.a35 ~loc (Actions.a34 ~loc (Actions.a1 ~loc a2_CODE ()) ()) a3_list ()) () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → actual COMMA separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_63 ~loc a0_separated_nonempty_list a2_actual _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a21 ~loc a0_separated_nonempty_list () (Actions.a33 ~loc a2_actual ()) () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       args → LPAREN separated_nonempty_list . RPAREN 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       RPAREN -> 65
     ACTION:
       RPAREN -> shift *)
  and state_64 ~loc a0_separated_nonempty_list _c0_args =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_65 ~loc a0_separated_nonempty_list _c0_args
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       args → LPAREN separated_nonempty_list RPAREN . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_65 ~loc a1_separated_nonempty_list _c0_args =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a36 ~loc () a1_separated_nonempty_list () () in
      _c0_args ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       loption → args . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_66 ~loc a0_args _c0_loption =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a23 ~loc a0_args () in
      _c0_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → symbol loption . 		/ ID, TID, CODE, DPREC, COMMA, PLUS, QMARK, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DPREC COMMA PLUS QMARK STAR RPAREN -> reduce 0 0 *)
  and state_67 ~loc a0_loption a1_symbol _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC | COMMA | PLUS | QMARK | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a37 ~loc a0_loption a1_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "STAR"; "RPAREN" ]

  (* ITEMS:
       producer → ID EQ actual . 		/ ID, TID, CODE, DPREC
       actual → actual . shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . PLUS 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . STAR 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       shorthand → . QMARK 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
     GOTO:
       PLUS -> 52
       QMARK -> 53
       STAR -> 54
       shorthand -> 55
     ACTION:
       ID TID CODE DPREC -> reduce 0 0
       PLUS QMARK STAR -> shift *)
  and state_68 ~loc a0_actual a2_ID _c0_producer _c1_actual =
    let rec _c2_shorthand ~loc x = state_55 ~loc x a0_actual _c1_actual in
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a38 ~loc a0_actual (Actions.a28 ~loc (Actions.a26 ~loc () (Actions.a9 ~loc (Actions.a1 ~loc a2_ID ()) ()) ()) ()) () in
      _c0_producer ~loc x
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_52 ~loc _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_53 ~loc _c2_shorthand
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_54 ~loc _c2_shorthand
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC"; "PLUS"; "QMARK"; "STAR" ]

  (* ITEMS:
       separated_nonempty_list → production . BAR separated_nonempty_list 		/ SEMI
       separated_nonempty_list → production . 		/ SEMI
     GOTO:
       BAR -> 70
     ACTION:
       BAR -> shift
       SEMI -> reduce 0 1 *)
  and state_69 ~loc a0_production _c0_separated_nonempty_list =
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_70 ~loc a0_production _c0_separated_nonempty_list
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a20 ~loc a0_production () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       separated_nonempty_list → production BAR . separated_nonempty_list 		/ SEMI
       symbol → . ID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR, LPAREN
       production → . list option CODE 		/ BAR, SEMI
       producer → . ID EQ actual 		/ ID, TID, CODE, DPREC
       producer → . actual 		/ ID, TID, CODE, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DPREC, PLUS, QMARK, STAR
       list → . producer list 		/ CODE, DPREC
       list → . 		/ CODE, DPREC
       separated_nonempty_list → . production BAR separated_nonempty_list 		/ SEMI
       separated_nonempty_list → . production 		/ SEMI
     GOTO:
       ID -> 46
       TID -> 11
       symbol -> 48
       production -> 69
       producer -> 50
       actual -> 51
       list -> 71
       separated_nonempty_list -> 76
     ACTION:
       CODE DPREC -> reduce 5 1
       ID TID -> shift *)
  and state_70 ~loc a1_production _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_48 ~loc x _c4_actual
    and _c2_production ~loc x = state_69 ~loc x _c6_separated_nonempty_list
    and _c3_producer ~loc x = state_50 ~loc x _c5_list
    and _c4_actual ~loc x = state_51 ~loc x _c3_producer _c4_actual
    and _c5_list ~loc x = state_71 ~loc x _c2_production
    and _c6_separated_nonempty_list ~loc x = state_76 ~loc x a1_production _c0_separated_nonempty_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_46 ~loc x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DPREC" ]

  (* ITEMS:
       production → list . option CODE 		/ BAR, SEMI
       option → . DPREC symbol 		/ CODE
       option → . 		/ CODE
     GOTO:
       DPREC -> 72
       option -> 74
     ACTION:
       DPREC -> shift
       CODE -> reduce 1 1 *)
  and state_71 ~loc a0_list _c0_production =
    let rec _c1_option ~loc x = state_74 ~loc x a0_list _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_72 ~loc _c1_option
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "CODE"; "DPREC" ]

  (* ITEMS:
       option → DPREC . symbol 		/ CODE
       symbol → . ID 		/ CODE
       symbol → . TID 		/ CODE
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 73
     ACTION:
       ID TID -> shift *)
  and state_72 ~loc _c0_option =
    let rec _c1_symbol ~loc x = state_73 ~loc x _c0_option in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       option → DPREC symbol . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_73 ~loc a0_symbol _c0_option =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a4 ~loc (Actions.a39 ~loc a0_symbol () ()) () in
      _c0_option ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       production → list option . CODE 		/ BAR, SEMI
     GOTO:
       CODE -> 75
     ACTION:
       CODE -> shift *)
  and state_74 ~loc a0_option a1_list _c0_production =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_75 ~loc x a0_option a1_list _c0_production
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       production → list option CODE . 		/ BAR, SEMI
     GOTO:
       
     ACTION:
       BAR SEMI -> reduce 0 0 *)
  and state_75 ~loc a0_CODE a1_option a2_list _c0_production =
    match lookahead () with
    (* Reduce *)
    | BAR | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a40 ~loc (Actions.a34 ~loc (Actions.a1 ~loc a0_CODE ()) ()) a1_option a2_list () in
      _c0_production ~loc x
    | _ -> fail [ "BAR"; "SEMI" ]

  (* ITEMS:
       separated_nonempty_list → production BAR separated_nonempty_list . 		/ SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_76 ~loc a0_separated_nonempty_list a2_production _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a21 ~loc a0_separated_nonempty_list () a2_production () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule → boption ID loption COLON option separated_nonempty_list . SEMI 		/ ID, DINLINE, EOF
     GOTO:
       SEMI -> 78
     ACTION:
       SEMI -> shift *)
  and state_77 ~loc a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_78 ~loc a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule
    | _ -> fail [ "SEMI" ]

  (* ITEMS:
       rule → boption ID loption COLON option separated_nonempty_list SEMI . 		/ ID, DINLINE, EOF
     GOTO:
       
     ACTION:
       ID DINLINE EOF -> reduce 0 0 *)
  and state_78 ~loc a1_separated_nonempty_list a2_option a4_loption a5_ID a6_boption _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | EOF ->
      let loc = loc_reduce ~loc 7
      and x = Actions.a41 ~loc () a1_separated_nonempty_list a2_option () a4_loption (Actions.a9 ~loc (Actions.a1 ~loc a5_ID ()) ()) a6_boption () in
      _c0_rule ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       list → rule list . 		/ EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_79 ~loc a0_list a1_rule _c0_list =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_rule () in
      _c0_list ~loc x
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list . EOF
     GOTO:
       EOF -> 81
     ACTION:
       EOF -> shift *)
  and state_80 ~loc a0_list a2_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_81 ~loc a0_list a2_list _c0_grammar_starting
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list EOF .
     GOTO:
       
     ACTION:
        *)
  and state_81 ~loc a1_list a3_list _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a42 ~loc () a1_list () a3_list () in
    _c0_grammar_starting x
  ;;
end

let grammar lexfun lexbuf =
  States.setup lexfun lexbuf;
  States.state_0 ~loc:[] (fun x -> x)
;;

let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens
