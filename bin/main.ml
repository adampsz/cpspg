let usage = "usage: cpspg [options] sourcefile"
let source_name = ref None
let output_name = ref None
let output_automaton = ref None
let grammar_kind = ref Cpspg.AutomatonGen.LALR
let codegen_comments = ref false
let codegen_readable_ids = ref false

let specs =
  [ ( "-o"
    , Arg.String (fun x -> output_name := Some x)
    , "<file> Set output file name to <file>" )
  ; ( "--automaton"
    , Arg.String (fun x -> output_automaton := Some x)
    , "<file> Dump automaton graph in .dot format to <file>" )
    (* Grammar kind *)
  ; ( "--lr0"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.AutomatonGen.LR0)
    , "Construct a LR(0) automaton" )
  ; ( "--slr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.AutomatonGen.SLR)
    , "Construct a SLR(1) automaton" )
  ; ( "--lr1"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.AutomatonGen.LR1)
    , "Construct a LR(1) automaton" )
  ; ( "--lalr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.AutomatonGen.LALR)
    , "Construct a LALR(1) automaton (default)" )
    (* Codegen options *)
  ; "--comment", Arg.Set codegen_comments, "Include comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "Make identifiers in generated code longer" )
  ]
  |> Arg.align
;;

let _ = Arg.parse specs (fun x -> source_name := Some x) usage

let print_conflicts term conflicts =
  let iter (id, sym, moves) =
    let sym =
      match sym with
      | Cpspg.Automaton.Follow.Term t -> (term t).Cpspg.Automaton.ti_name
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
  List.iter iter conflicts
;;

let output_automaton (module A : Cpspg.AutomatonGen.Automaton) =
  match !output_automaton with
  | None -> ()
  | Some "-" ->
    let module G = Cpspg.Graphviz.Make (A) in
    G.fmt_automaton Format.std_formatter A.automaton
  | Some x ->
    let module G = Cpspg.Graphviz.Make (A) in
    G.fmt_automaton (Format.formatter_of_out_channel (open_out x)) A.automaton
;;

let output_code (module A : Cpspg.AutomatonGen.Automaton) =
  let f =
    match !output_name with
    | None | Some "-" -> Format.std_formatter
    | Some x -> Format.formatter_of_out_channel (open_out x)
  in
  let module Settings = struct
    let comments = !codegen_comments
    let readable_ids = !codegen_readable_ids
    let log = Format.eprintf
  end
  in
  let module Input = struct
    include A

    let f = f
  end
  in
  let module C = Cpspg.CodeGen.Run (Settings) (Input) in
  ()
;;

let main () =
  let lexbuf =
    match !source_name with
    | None | Some "-" -> Lexing.from_channel stdin
    | Some x -> Lexing.from_channel (open_in x)
  and lexfun lexbuf =
    match Cpspg.Lexer.token lexbuf with
    | Cpspg.Parser.EOF -> None
    | t -> Some t
  in
  let grammar = Cpspg.Parser.grammar lexbuf lexfun in
  let conflicts = ref [] in
  (* Generate automaton *)
  let module Settings = struct
    let kind = !grammar_kind
    let on_conflict id sym moves = conflicts := (id, sym, moves) :: !conflicts
    let log = Format.eprintf
  end
  in
  let module Input = struct
    let grammar = grammar
  end
  in
  let module A = Cpspg.AutomatonGen.Run (Settings) (Input) in
  print_conflicts A.term !conflicts;
  output_automaton (module A);
  output_code (module A)
;;

let _ =
  main ();
  exit 0
;;
