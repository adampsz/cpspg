let parse lexfun lexbuf =
  try Parser.chunk lexfun lexbuf with
  | e ->
    let open Lexing in
    let pos = lexbuf.lex_start_p in
    let f, l, c = pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol in
    Format.eprintf "File \"%s\", line %d, character %d:\n" f l c;
    Format.eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1
;;

let lex lexfun lexbuf =
  let open Lexing in
  let rec loop acc =
    match lexfun lexbuf with
    | Parser.EOF -> List.rev acc
    | token -> loop ((token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: acc)
  in
  let lexfun input lexbuf =
    match !input with
    | [] -> Parser.EOF
    | (token, start, curr) :: xs ->
      input := xs;
      lexbuf.lex_start_p <- start;
      lexbuf.lex_curr_p <- curr;
      token
  in
  let input = loop [] in
  fun () ->
    let input = ref input
    and lexbuf = Lexing.from_string ""
    and filename = lexbuf.lex_curr_p.pos_fname in
    set_filename lexbuf filename;
    lexfun input, lexbuf
;;

let bench input =
  let start = Unix.gettimeofday () in
  let lex = lex Lexer.token (Lexing.from_string input) in
  let chunk = ref [] in
  for _ = 1 to 100 do
    let lexfun, lexbuf = lex () in
    chunk := parse lexfun lexbuf
  done;
  Unix.gettimeofday () -. start, !chunk
;;

let _ =
  let input = In_channel.input_all stdin in
  let time, chunk = bench input in
  (* Ast.pp_block Format.std_formatter chunk; *)
  Format.eprintf "Time: %fs\n%!" time
;;
