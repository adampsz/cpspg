let _ =
  let lexbuf = Lexing.from_channel stdin in
  Printf.printf "%d\n" (Parser.start Lexer.token lexbuf)
;;
