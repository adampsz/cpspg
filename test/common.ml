let lexbuf = Lexing.from_string ""

let lex tokens =
  let tokens = ref tokens in
  let next _ =
    let hd = List.hd !tokens in
    tokens := List.tl !tokens;
    hd
  in
  next
;;

let check t parse name tok res =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check t) name (parse (lex tok) lexbuf) res)
;;
