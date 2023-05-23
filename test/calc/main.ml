let _ =
  let rec loop () =
    Printf.printf "> %!";
    match In_channel.input_line stdin with
    | None -> ()
    | Some line when String.trim line = "" -> loop ()
    | Some line ->
      let lexbuf = Lexing.from_string line in
      Printf.printf "= %d\n%!" (Parser.main Lexer.token lexbuf);
      loop ()
  in
  loop ()
;;
