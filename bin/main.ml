let usage = "usage: cpspg [options] sourcefile"
let source_name = ref None
let output_name = ref None
let output_automaton = ref None
let grammar_kind = ref Cpspg.Types.LALR
let codegen_line_directives = ref true
let codegen_comments = ref false
let codegen_readable_ids = ref false

let codegen_readable () =
  codegen_line_directives := false;
  codegen_readable_ids := true;
  codegen_comments := true
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
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR0)
    , "Construct a LR(0) automaton" )
  ; ( "--slr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.SLR)
    , "Construct a SLR(1) automaton" )
  ; ( "--lr1"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR1)
    , "Construct a LR(1) automaton" )
  ; ( "--lalr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LALR)
    , "Construct a LALR(1) automaton (default)" )
    (* Codegen options *)
  ; ( "--no-positions"
    , Arg.Unit (fun _ -> codegen_comments := false)
    , "Disable $loc family of keywords and related code" )
  ; ( "--no-line-directives"
    , Arg.Unit (fun _ -> codegen_line_directives := false)
    , "Do not include line directives in generated code" )
  ; "--comment", Arg.Set codegen_comments, "Include comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "Make identifiers in generated code longer" )
  ; ( "--readable"
    , Arg.Unit codegen_readable
    , "Make generated code more readable (implies --comment, --readable-ids and \
       --no-line-directives)" )
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

let main () =
  let input, input_name =
    match !source_name with
    | None | Some "-" -> stdin, "-"
    | Some x -> open_in x, x
  and output =
    match !output_name with
    | None | Some "-" -> stdout
    | Some x -> open_out x
  in
  let conflicts = ref [] in
  (* Settings *)
  let module Settings = struct
    let kind = !grammar_kind
    let positions = !codegen_comments
    let line_directives = !codegen_line_directives
    let comments = !codegen_comments
    let readable_ids = !codegen_readable_ids
    let on_conflict id sym actions = conflicts := (id, sym, actions) :: !conflicts
  end
  in
  (* First pass: parse grammar definition *)
  let module Ast = struct
    let lexbuf = Lexing.from_channel input
    let _ = Lexing.set_filename lexbuf input_name
    let ast = Cpspg.Parser.grammar Cpspg.Lexer.main lexbuf
  end
  in
  (* Second pass: create context-free grammar *)
  let module Grammar = Cpspg.GrammarGen.Run (Settings) (Ast) in
  (* Third pass: create LR automaton *)
  let module Automaton = Cpspg.AutomatonGen.Run (Settings) (Grammar) in
  (* Fourth pass: initialize code generation *)
  let module Code = Cpspg.CodeGen.Make (Settings) (Grammar) (Automaton) in
  let module Graphviz = Cpspg.Graphviz.Make (Grammar) in
  (* Final work *)
  Code.write (Format.formatter_of_out_channel output);
  print_conflicts Grammar.term !conflicts
;;

let _ =
  main ();
  exit 0
;;
