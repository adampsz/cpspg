let usage = "usage: cpspg [options] <filename>"
let source_name = ref None
let output_name = ref None
let output_format = ref ".ml"
let grammar_kind = ref Cpspg.Types.LALR
let codegen_line_directives = ref true
let codegen_comments = ref false
let codegen_readable_ids = ref false
let codegen_locations = ref true

let codegen_readable () =
  codegen_line_directives := false;
  codegen_readable_ids := true;
  codegen_comments := true
;;

let specs =
  [ ( "-o"
    , Arg.String
        (fun x ->
          output_name := Some x;
          output_format := Filename.extension x)
    , "<file>\tSet output file name to <file>" )
  ; ( "-f"
    , Arg.String (fun x -> output_format := "." ^ x)
    , "<format>\tSet output format to <format> (default: detect)" )
    (* Grammar kind *)
  ; ( "--lr0"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR0)
    , "\tConstruct a LR(0) automaton" )
  ; ( "--slr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.SLR)
    , "\tConstruct a SLR(1) automaton" )
  ; ( "--lr1"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR1)
    , "\tConstruct a LR(1) automaton" )
  ; ( "--lalr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LALR)
    , "\tConstruct a LALR(1) automaton (default)" )
    (* Codegen options *)
  ; ( "--no-locations"
    , Arg.Unit (fun _ -> codegen_locations := false)
    , "\tDisable family of $loc keywords and related code" )
  ; ( "--no-line-directives"
    , Arg.Unit (fun _ -> codegen_line_directives := false)
    , "\tDo not include line directives in generated code" )
  ; "--comment", Arg.Set codegen_comments, "\tInclude comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "\tMake identifiers in generated code longer" )
  ; ( "--readable"
    , Arg.Unit codegen_readable
    , "\tMake generated code more readable (implies --comment, --readable-ids and \
       --no-line-directives)" )
  ]
  |> Arg.align
;;

let _ = Arg.parse specs (fun x -> source_name := Some x) usage

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

    (* Codegen *)
    let locations = !codegen_locations
    let line_directives = !codegen_line_directives
    let comments = !codegen_comments
    let readable_ids = !codegen_readable_ids

    (* Reporting *)
    let report_err ~loc = Format.kdprintf (Warning.report_err ~loc:(fst loc))
    let report_warn ~loc = Format.kdprintf (Warning.report_warn ~loc:(fst loc))
    let report_conflict id sym actions = conflicts := (id, sym, actions) :: !conflicts
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
  let module Conflicts = Warning.Conflict (Grammar) (Automaton) in
  List.iter (fun (i, s, a) -> Conflicts.report i s a) !conflicts;
  (* Fourth pass: initialize code generation *)
  let module Code =
    (val match !output_format with
         | ".ml" -> (module Cpspg.CodeGenMl.Make (Settings) (Grammar) (Automaton))
         | ".dot" -> (module Cpspg.CodeGenDot.Make (Settings) (Grammar) (Automaton))
         | _ -> failwith "Unknown output format"
      : Cpspg.Types.Code)
  in
  (* Write results *)
  Code.write (Format.formatter_of_out_channel output)
;;

let _ =
  main ();
  exit 0
;;
