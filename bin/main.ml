type mode =
  | Code
  | Automaton

let usage = "cpspg\ncpspg -A"
let mode = ref Code
let grammar_kind = ref Cpspg.AutomatonGen.LALR
let gen_comments = ref false
let gen_readable_ids = ref false

let set_grammar_kind = function
  | "LR0" -> grammar_kind := Cpspg.AutomatonGen.LR0
  | "SLR" -> grammar_kind := Cpspg.AutomatonGen.SLR
  | "LR1" -> grammar_kind := Cpspg.AutomatonGen.LR1
  | "LALR" -> grammar_kind := Cpspg.AutomatonGen.LALR
  | _ -> failwith "Unknown grammar type"
;;

let speclist =
  [ ( "-A"
    , Arg.Unit (fun _ -> mode := Automaton)
    , "Output automaton graph in .dot format instead of code" )
  ; "--grammar-kind", Arg.String set_grammar_kind, "Change grammar type [default: LALR]"
  ; ( "--gen-comments"
    , Arg.Unit (fun _ -> gen_comments := true)
    , "Include comments in generated code" )
  ; ( "--gen-readable-ids"
    , Arg.Unit (fun _ -> gen_readable_ids := true)
    , "Generate readable ids in code" )
  ]
;;

let _ =
  Arg.parse speclist (fun _ -> failwith "Unexpected argument") usage;
  let lexbuf = Lexing.from_channel stdin
  and lexfun lexbuf =
    match Cpspg.Lexer.token lexbuf with
    | Cpspg.Parser.EOF -> None
    | t -> Some t
  in
  let grammar = Cpspg.Parser.grammar lexbuf lexfun in
  let conflicts = ref [] in
  (* Generate automaton *)
  let module A =
    Cpspg.AutomatonGen.Run
      (struct
        let kind = !grammar_kind
        let on_conflict id sym moves = conflicts := (id, sym, moves) :: !conflicts
        let log = Format.eprintf
      end)
      (struct
        let grammar = grammar
      end)
  in
  (* Print conflicts *)
  let _ =
    let iter (id, sym, moves) =
      let sym =
        match sym with
        | Cpspg.Automaton.Follow.Term t -> (A.term t).ti_name
        | Cpspg.Automaton.Follow.End -> "$"
      in
      Format.eprintf "Conflict in state %d on symbol %s:\n" id sym;
      let f = function
        | Cpspg.Automaton.Shift -> Format.eprintf "  - shift\n"
        | Cpspg.Automaton.Reduce (i, j) ->
          Format.eprintf "  - reduce item %d in group %d\n" i j
      in
      List.iter f moves;
      Format.eprintf "%!"
    in
    List.iter iter !conflicts
  in
  match !mode with
  | Code ->
    let module C =
      Cpspg.CodeGen.Run
        (struct
          let comments = !gen_comments
          let readable_ids = !gen_readable_ids
          let log = Format.eprintf
        end)
        (struct
          include A

          let f = Format.std_formatter
        end)
    in
    ()
  | Automaton ->
    let module G = Cpspg.Graphviz.Make (A) in
    G.fmt_automaton Format.std_formatter A.automaton
;;
