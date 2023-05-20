let _ =
  let lexbuf = Lexing.from_channel stdin
  and lexfun lexbuf =
    match Lexer.token lexbuf with
    | Lexer.EOF -> None
    | t -> Some t
  in
  Printf.printf "%d\n" (Parser.expression lexbuf lexfun)
;;
