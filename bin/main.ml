let usage = "usage: cpspg [options] sourcefile"
let source_name = ref None
let output_name = ref None
let output_automaton = ref None
let grammar_kind = ref Cpspg.AutomatonGen.LALR
let codegen_comment = ref false
let codegen_readable_ids = ref false
let codegen_line_directives = ref true

let codegen_readable () =
  codegen_readable_ids := true;
  codegen_comment := true;
  codegen_line_directives := false
;;

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
  ; ( "--readable"
    , Arg.Unit codegen_readable
    , "Make generated code more readable (implies --comment, --readable-ids and \
       --no-line-directives)" )
  ; "--comment", Arg.Set codegen_comment, "Include comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "Make identifiers in generated code longer" )
  ; ( "--no-line-directives"
    , Arg.Unit (fun _ -> codegen_line_directives := false)
    , "Do not include line directives in generated code" )
  ]
  |> Arg.align
;;

let _ = Arg.parse specs (fun x -> source_name := Some x) usage

let print_conflicts term conflicts =
  let iter (id, sym, actions) =
    let sym = (term sym).Cpspg.Automaton.ti_name in
    Format.eprintf "Conflict in state %d on symbol %s:\n" id sym;
    let f = function
      | Cpspg.Automaton.Shift -> Format.eprintf "  - shift\n"
      | Cpspg.Automaton.Reduce (i, j) ->
        Format.eprintf "  - reduce item %d in group %d\n" i j
    in
    List.iter f actions;
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
    let comments = !codegen_comment
    let readable_ids = !codegen_readable_ids
    let line_directives = !codegen_line_directives
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
  in
  let grammar = Cpspg.Parser.grammar lexbuf Cpspg.Lexer.main in
  let conflicts = ref [] in
  (* Generate automaton *)
  let module Settings = struct
    let kind = !grammar_kind
    let on_conflict id sym actions = conflicts := (id, sym, actions) :: !conflicts
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
