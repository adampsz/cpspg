[@@@warning "-unused-rec-flag"]

open Grammar

type token =
  | ID of string
  | TID of string
  | TYPE of string
  | CODE of string
  | DTOKEN
  | DTYPE
  | DSTART
  | DLEFT
  | DRIGHT
  | DNONASSOC
  | DSEP
  | COLON
  | SEMI
  | BAR
  | EQ
  | EOF

module Actions = struct
  let a1_grammar rules decls header () = { header; decls; rules }
  let a2_header header () = header
  let a3_decls decls decl () = decl :: decls
  let a4_decls () = []
  let a5_decl tids tp () = DeclToken (Some tp, tids)
  let a6_decl ids tp () = DeclType (tp, ids)
  let a7_decl tids () = DeclToken (None, tids)
  let a8_decl ids () = DeclStart ids
  let a9_decl ids () = DeclLeft ids
  let a10_decl ids () = DeclRight ids
  let a11_decl ids () = DeclNonassoc ids
  let a12_tids tids id () = id :: tids
  let a13_tids () = []
  let a14_ids ids id () = id :: ids
  let a15_ids () = []
  let a16_rules rules rule () = rule :: rules
  let a17_rules () = []
  let a18_rule prods id () = { id; prods }
  let a19_rule_prods productions production () = production :: productions
  let a20_rule_prods productions () = productions
  let a21_productions productions production () = production :: productions
  let a22_productions () = []
  let a23_production action prod () = { prod; action }
  let a24_producers producers producer () = producer :: producers
  let a25_producers () = []
  let a26_producer actual id () = { id = Some id; actual }
  let a27_producer actual () = { id = None; actual }
  let a28_actual name () = NTerm name
  let a29_actual name () = Term name
end

module States = struct
  let lexfun = ref (fun _ -> assert false)
  let lexbuf = ref (Lexing.from_string String.empty)
  let lookahead = ref None

  let setup lf lb =
    lexfun := lf;
    lexbuf := lb;
    lookahead := None
  ;;

  let shift () =
    let t = Option.get !lookahead in
    lookahead := None;
    t
  ;;

  let lookahead () =
    match !lookahead with
    | Some t -> t
    | None ->
      let t = !lexfun !lexbuf in
      lookahead := Some t;
      t
  ;;

  (* ITEMS:
       grammar' → . header decls DSEP rules EOF
       header → . CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       CODE -> 1
       header -> 2
     ACTION:
       CODE -> shift *)
  let rec state_0 c0_grammar_starting =
    let rec c1_header x = state_2 x c0_grammar_starting in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _ = shift () in
      state_1 x c1_header
    | _ -> raise (Failure "error in state 0")

  (* ITEMS:
       header → CODE . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_1 a0_CODE c0_header =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a2_header a0_CODE () in
      c0_header x
    | _ -> raise (Failure "error in state 1")

  (* ITEMS:
       grammar' → header . decls DSEP rules EOF
       decls → . decl decls /DSEP
       decls → . /DSEP
       decl → . DTOKEN TYPE tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTYPE TYPE ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTOKEN tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DLEFT ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DRIGHT ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DNONASSOC ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       DTOKEN -> 3
       DTYPE -> 9
       DSTART -> 14
       DLEFT -> 16
       DRIGHT -> 18
       DNONASSOC -> 20
       decls -> 22
       decl -> 48
     ACTION:
       DSEP -> reduce 1 1
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  and state_2 a0_header c0_grammar_starting =
    let rec c1_decls x = state_22 x a0_header c0_grammar_starting
    and c2_decl x = state_48 x c1_decls in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let x = Actions.a4_decls () in
      c1_decls x
    (* Shift *)
    | DTOKEN ->
      let _ = shift () in
      state_3 c2_decl
    (* Shift *)
    | DTYPE ->
      let _ = shift () in
      state_9 c2_decl
    (* Shift *)
    | DSTART ->
      let _ = shift () in
      state_14 c2_decl
    (* Shift *)
    | DLEFT ->
      let _ = shift () in
      state_16 c2_decl
    (* Shift *)
    | DRIGHT ->
      let _ = shift () in
      state_18 c2_decl
    (* Shift *)
    | DNONASSOC ->
      let _ = shift () in
      state_20 c2_decl
    | _ -> raise (Failure "error in state 2")

  (* ITEMS:
       decl → DTOKEN . TYPE tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → DTOKEN . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . TID tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       TYPE -> 6
       tids -> 8
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       TID TYPE -> shift *)
  and state_3 c0_decl =
    let rec c1_tids x = state_8 x c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a13_tids () in
      c1_tids x
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_4 x c1_tids
    (* Shift *)
    | TYPE x ->
      let _ = shift () in
      state_6 x c0_decl
    | _ -> raise (Failure "error in state 3")

  (* ITEMS:
       tids → TID . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . TID tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       tids -> 5
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       TID -> shift *)
  and state_4 a0_TID c0_tids =
    let rec c1_tids x = state_5 x a0_TID c0_tids in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a13_tids () in
      c1_tids x
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_4 x c1_tids
    | _ -> raise (Failure "error in state 4")

  (* ITEMS:
       tids → TID tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_5 a0_tids a1_TID c0_tids =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a12_tids a0_tids a1_TID () in
      c0_tids x
    | _ -> raise (Failure "error in state 5")

  (* ITEMS:
       decl → DTOKEN TYPE . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . TID tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       tids -> 7
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       TID -> shift *)
  and state_6 a0_TYPE c0_decl =
    let rec c1_tids x = state_7 x a0_TYPE c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a13_tids () in
      c1_tids x
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_4 x c1_tids
    | _ -> raise (Failure "error in state 6")

  (* ITEMS:
       decl → DTOKEN TYPE tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_7 a0_tids a1_TYPE c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a5_decl a0_tids a1_TYPE () in
      c0_decl x
    | _ -> raise (Failure "error in state 7")

  (* ITEMS:
       decl → DTOKEN tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_8 a0_tids c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a7_decl a0_tids () in
      c0_decl x
    | _ -> raise (Failure "error in state 8")

  (* ITEMS:
       decl → DTYPE . TYPE ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TYPE -> 10
     ACTION:
       TYPE -> shift *)
  and state_9 c0_decl =
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _ = shift () in
      state_10 x c0_decl
    | _ -> raise (Failure "error in state 9")

  (* ITEMS:
       decl → DTYPE TYPE . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 13
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_10 a0_TYPE c0_decl =
    let rec c1_ids x = state_13 x a0_TYPE c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 10")

  (* ITEMS:
       ids → ID . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 12
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_11 a0_ID c0_ids =
    let rec c1_ids x = state_12 x a0_ID c0_ids in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 11")

  (* ITEMS:
       ids → ID ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_12 a0_ids a1_ID c0_ids =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a14_ids a0_ids a1_ID () in
      c0_ids x
    | _ -> raise (Failure "error in state 12")

  (* ITEMS:
       decl → DTYPE TYPE ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_13 a0_ids a1_TYPE c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a6_decl a0_ids a1_TYPE () in
      c0_decl x
    | _ -> raise (Failure "error in state 13")

  (* ITEMS:
       decl → DSTART . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 15
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_14 c0_decl =
    let rec c1_ids x = state_15 x c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 14")

  (* ITEMS:
       decl → DSTART ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_15 a0_ids c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a8_decl a0_ids () in
      c0_decl x
    | _ -> raise (Failure "error in state 15")

  (* ITEMS:
       decl → DLEFT . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 17
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_16 c0_decl =
    let rec c1_ids x = state_17 x c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 16")

  (* ITEMS:
       decl → DLEFT ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_17 a0_ids c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a9_decl a0_ids () in
      c0_decl x
    | _ -> raise (Failure "error in state 17")

  (* ITEMS:
       decl → DRIGHT . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 19
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_18 c0_decl =
    let rec c1_ids x = state_19 x c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 18")

  (* ITEMS:
       decl → DRIGHT ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_19 a0_ids c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a10_decl a0_ids () in
      c0_decl x
    | _ -> raise (Failure "error in state 19")

  (* ITEMS:
       decl → DNONASSOC . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . ID ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 11
       ids -> 21
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
       ID -> shift *)
  and state_20 c0_decl =
    let rec c1_ids x = state_21 x c0_decl in
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a15_ids () in
      c1_ids x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_11 x c1_ids
    | _ -> raise (Failure "error in state 20")

  (* ITEMS:
       decl → DNONASSOC ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_21 a0_ids c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a11_decl a0_ids () in
      c0_decl x
    | _ -> raise (Failure "error in state 21")

  (* ITEMS:
       grammar' → header decls . DSEP rules EOF
     GOTO:
       DSEP -> 23
     ACTION:
       DSEP -> shift *)
  and state_22 a0_decls a1_header c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let _ = shift () in
      state_23 a0_decls a1_header c0_grammar_starting
    | _ -> raise (Failure "error in state 22")

  (* ITEMS:
       grammar' → header decls DSEP . rules EOF
       rules → . rule rules /EOF
       rules → . /EOF
       rule → . ID COLON rule_prods SEMI /ID /EOF
     GOTO:
       ID -> 24
       rules -> 44
       rule -> 46
     ACTION:
       EOF -> reduce 1 1
       ID -> shift *)
  and state_23 a1_decls a2_header c0_grammar_starting =
    let rec c1_rules x = state_44 x a1_decls a2_header c0_grammar_starting
    and c2_rule x = state_46 x c1_rules in
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let x = Actions.a17_rules () in
      c1_rules x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_24 x c2_rule
    | _ -> raise (Failure "error in state 23")

  (* ITEMS:
       rule → ID . COLON rule_prods SEMI /ID /EOF
     GOTO:
       COLON -> 25
     ACTION:
       COLON -> shift *)
  and state_24 a0_ID c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _ = shift () in
      state_25 a0_ID c0_rule
    | _ -> raise (Failure "error in state 24")

  (* ITEMS:
       rule → ID COLON . rule_prods SEMI /ID /EOF
       rule_prods → . production productions /SEMI
       rule_prods → . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
       production → . producers CODE /SEMI /BAR
       producers → . producer producers /CODE
       producers → . /CODE
       producer → . ID EQ actual /ID /TID /CODE
       producer → . actual /ID /TID /CODE
       actual → . ID /ID /TID /CODE
       actual → . TID /ID /TID /CODE
     GOTO:
       ID -> 26
       TID -> 29
       BAR -> 31
       rule_prods -> 39
       productions -> 41
       production -> 42
       producers -> 34
       producer -> 36
       actual -> 38
     ACTION:
       CODE -> reduce 4 1
       SEMI -> reduce 2 1
       ID TID BAR -> shift *)
  and state_25 a1_ID c0_rule =
    let rec c1_rule_prods x = state_39 x a1_ID c0_rule
    and c2_productions x = state_41 x c1_rule_prods
    and c3_production x = state_42 x c1_rule_prods
    and c4_producers x = state_34 x c3_production
    and c5_producer x = state_36 x c4_producers
    and c6_actual x = state_38 x c5_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a25_producers () in
      c4_producers x
    (* Reduce *)
    | SEMI ->
      let x = Actions.a22_productions () in
      c2_productions x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_26 x c5_producer c6_actual
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_29 x c6_actual
    (* Shift *)
    | BAR ->
      let _ = shift () in
      state_31 c2_productions
    | _ -> raise (Failure "error in state 25")

  (* ITEMS:
       producer → ID . EQ actual /ID /TID /CODE
       actual → ID . /ID /TID /CODE
     GOTO:
       EQ -> 27
     ACTION:
       ID TID CODE -> reduce 1 0
       EQ -> shift *)
  and state_26 a0_ID c0_producer c1_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let x = Actions.a28_actual a0_ID () in
      c1_actual x
    (* Shift *)
    | EQ ->
      let _ = shift () in
      state_27 a0_ID c0_producer
    | _ -> raise (Failure "error in state 26")

  (* ITEMS:
       producer → ID EQ . actual /ID /TID /CODE
       actual → . ID /ID /TID /CODE
       actual → . TID /ID /TID /CODE
     GOTO:
       ID -> 28
       TID -> 29
       actual -> 30
     ACTION:
       ID TID -> shift *)
  and state_27 a1_ID c0_producer =
    let rec c1_actual x = state_30 x a1_ID c0_producer in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_28 x c1_actual
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_29 x c1_actual
    | _ -> raise (Failure "error in state 27")

  (* ITEMS:
       actual → ID . /ID /TID /CODE
     GOTO:
       
     ACTION:
       ID TID CODE -> reduce 0 0 *)
  and state_28 a0_ID c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let x = Actions.a28_actual a0_ID () in
      c0_actual x
    | _ -> raise (Failure "error in state 28")

  (* ITEMS:
       actual → TID . /ID /TID /CODE
     GOTO:
       
     ACTION:
       ID TID CODE -> reduce 0 0 *)
  and state_29 a0_TID c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let x = Actions.a29_actual a0_TID () in
      c0_actual x
    | _ -> raise (Failure "error in state 29")

  (* ITEMS:
       producer → ID EQ actual . /ID /TID /CODE
     GOTO:
       
     ACTION:
       ID TID CODE -> reduce 0 0 *)
  and state_30 a0_actual a2_ID c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let x = Actions.a26_producer a0_actual a2_ID () in
      c0_producer x
    | _ -> raise (Failure "error in state 30")

  (* ITEMS:
       productions → BAR . production productions /SEMI
       production → . producers CODE /SEMI /BAR
       producers → . producer producers /CODE
       producers → . /CODE
       producer → . ID EQ actual /ID /TID /CODE
       producer → . actual /ID /TID /CODE
       actual → . ID /ID /TID /CODE
       actual → . TID /ID /TID /CODE
     GOTO:
       ID -> 26
       TID -> 29
       production -> 32
       producers -> 34
       producer -> 36
       actual -> 38
     ACTION:
       CODE -> reduce 2 1
       ID TID -> shift *)
  and state_31 c0_productions =
    let rec c1_production x = state_32 x c0_productions
    and c2_producers x = state_34 x c1_production
    and c3_producer x = state_36 x c2_producers
    and c4_actual x = state_38 x c3_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a25_producers () in
      c2_producers x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_26 x c3_producer c4_actual
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_29 x c4_actual
    | _ -> raise (Failure "error in state 31")

  (* ITEMS:
       productions → BAR production . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
     GOTO:
       BAR -> 31
       productions -> 33
     ACTION:
       SEMI -> reduce 1 1
       BAR -> shift *)
  and state_32 a0_production c0_productions =
    let rec c1_productions x = state_33 x a0_production c0_productions in
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a22_productions () in
      c1_productions x
    (* Shift *)
    | BAR ->
      let _ = shift () in
      state_31 c1_productions
    | _ -> raise (Failure "error in state 32")

  (* ITEMS:
       productions → BAR production productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_33 a0_productions a1_production c0_productions =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a21_productions a0_productions a1_production () in
      c0_productions x
    | _ -> raise (Failure "error in state 33")

  (* ITEMS:
       production → producers . CODE /SEMI /BAR
     GOTO:
       CODE -> 35
     ACTION:
       CODE -> shift *)
  and state_34 a0_producers c0_production =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _ = shift () in
      state_35 x a0_producers c0_production
    | _ -> raise (Failure "error in state 34")

  (* ITEMS:
       production → producers CODE . /SEMI /BAR
     GOTO:
       
     ACTION:
       SEMI BAR -> reduce 0 0 *)
  and state_35 a0_CODE a1_producers c0_production =
    match lookahead () with
    (* Reduce *)
    | SEMI | BAR ->
      let x = Actions.a23_production a0_CODE a1_producers () in
      c0_production x
    | _ -> raise (Failure "error in state 35")

  (* ITEMS:
       producers → producer . producers /CODE
       producers → . producer producers /CODE
       producers → . /CODE
       producer → . ID EQ actual /ID /TID /CODE
       producer → . actual /ID /TID /CODE
       actual → . ID /ID /TID /CODE
       actual → . TID /ID /TID /CODE
     GOTO:
       ID -> 26
       TID -> 29
       producers -> 37
       producer -> 36
       actual -> 38
     ACTION:
       CODE -> reduce 1 1
       ID TID -> shift *)
  and state_36 a0_producer c0_producers =
    let rec c1_producers x = state_37 x a0_producer c0_producers
    and c2_producer x = state_36 x c1_producers
    and c3_actual x = state_38 x c2_producer in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a25_producers () in
      c1_producers x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_26 x c2_producer c3_actual
    (* Shift *)
    | TID x ->
      let _ = shift () in
      state_29 x c3_actual
    | _ -> raise (Failure "error in state 36")

  (* ITEMS:
       producers → producer producers . /CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_37 a0_producers a1_producer c0_producers =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a24_producers a0_producers a1_producer () in
      c0_producers x
    | _ -> raise (Failure "error in state 37")

  (* ITEMS:
       producer → actual . /ID /TID /CODE
     GOTO:
       
     ACTION:
       ID TID CODE -> reduce 0 0 *)
  and state_38 a0_actual c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let x = Actions.a27_producer a0_actual () in
      c0_producer x
    | _ -> raise (Failure "error in state 38")

  (* ITEMS:
       rule → ID COLON rule_prods . SEMI /ID /EOF
     GOTO:
       SEMI -> 40
     ACTION:
       SEMI -> shift *)
  and state_39 a0_rule_prods a2_ID c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _ = shift () in
      state_40 a0_rule_prods a2_ID c0_rule
    | _ -> raise (Failure "error in state 39")

  (* ITEMS:
       rule → ID COLON rule_prods SEMI . /ID /EOF
     GOTO:
       
     ACTION:
       ID EOF -> reduce 0 0 *)
  and state_40 a1_rule_prods a3_ID c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | EOF ->
      let x = Actions.a18_rule a1_rule_prods a3_ID () in
      c0_rule x
    | _ -> raise (Failure "error in state 40")

  (* ITEMS:
       rule_prods → productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_41 a0_productions c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a20_rule_prods a0_productions () in
      c0_rule_prods x
    | _ -> raise (Failure "error in state 41")

  (* ITEMS:
       rule_prods → production . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
     GOTO:
       BAR -> 31
       productions -> 43
     ACTION:
       SEMI -> reduce 1 1
       BAR -> shift *)
  and state_42 a0_production c0_rule_prods =
    let rec c1_productions x = state_43 x a0_production c0_rule_prods in
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a22_productions () in
      c1_productions x
    (* Shift *)
    | BAR ->
      let _ = shift () in
      state_31 c1_productions
    | _ -> raise (Failure "error in state 42")

  (* ITEMS:
       rule_prods → production productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_43 a0_productions a1_production c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a19_rule_prods a0_productions a1_production () in
      c0_rule_prods x
    | _ -> raise (Failure "error in state 43")

  (* ITEMS:
       grammar' → header decls DSEP rules . EOF
     GOTO:
       EOF -> 45
     ACTION:
       EOF -> shift *)
  and state_44 a0_rules a2_decls a3_header c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _ = shift () in
      state_45 a0_rules a2_decls a3_header c0_grammar_starting
    | _ -> raise (Failure "error in state 44")

  (* ITEMS:
       grammar' → header decls DSEP rules EOF .
     GOTO:
       
     ACTION:
       -> reduce 0 0 *)
  and state_45 a1_rules a3_decls a4_header c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a1_grammar a1_rules a3_decls a4_header () in
    c0_grammar_starting x

  (* ITEMS:
       rules → rule . rules /EOF
       rules → . rule rules /EOF
       rules → . /EOF
       rule → . ID COLON rule_prods SEMI /ID /EOF
     GOTO:
       ID -> 24
       rules -> 47
       rule -> 46
     ACTION:
       EOF -> reduce 1 1
       ID -> shift *)
  and state_46 a0_rule c0_rules =
    let rec c1_rules x = state_47 x a0_rule c0_rules
    and c2_rule x = state_46 x c1_rules in
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let x = Actions.a17_rules () in
      c1_rules x
    (* Shift *)
    | ID x ->
      let _ = shift () in
      state_24 x c2_rule
    | _ -> raise (Failure "error in state 46")

  (* ITEMS:
       rules → rule rules . /EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_47 a0_rules a1_rule c0_rules =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let x = Actions.a16_rules a0_rules a1_rule () in
      c0_rules x
    | _ -> raise (Failure "error in state 47")

  (* ITEMS:
       decls → decl . decls /DSEP
       decls → . decl decls /DSEP
       decls → . /DSEP
       decl → . DTOKEN TYPE tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTYPE TYPE ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTOKEN tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DLEFT ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DRIGHT ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DNONASSOC ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       DTOKEN -> 3
       DTYPE -> 9
       DSTART -> 14
       DLEFT -> 16
       DRIGHT -> 18
       DNONASSOC -> 20
       decls -> 49
       decl -> 48
     ACTION:
       DSEP -> reduce 1 1
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  and state_48 a0_decl c0_decls =
    let rec c1_decls x = state_49 x a0_decl c0_decls
    and c2_decl x = state_48 x c1_decls in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let x = Actions.a4_decls () in
      c1_decls x
    (* Shift *)
    | DTOKEN ->
      let _ = shift () in
      state_3 c2_decl
    (* Shift *)
    | DTYPE ->
      let _ = shift () in
      state_9 c2_decl
    (* Shift *)
    | DSTART ->
      let _ = shift () in
      state_14 c2_decl
    (* Shift *)
    | DLEFT ->
      let _ = shift () in
      state_16 c2_decl
    (* Shift *)
    | DRIGHT ->
      let _ = shift () in
      state_18 c2_decl
    (* Shift *)
    | DNONASSOC ->
      let _ = shift () in
      state_20 c2_decl
    | _ -> raise (Failure "error in state 48")

  (* ITEMS:
       decls → decl decls . /DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_49 a0_decls a1_decl c0_decls =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let x = Actions.a3_decls a0_decls a1_decl () in
      c0_decls x
    | _ -> raise (Failure "error in state 49")
  ;;
end

let grammar lexbuf lexfun =
  States.setup lexfun lexbuf;
  States.state_0 (fun x -> x)
;;
