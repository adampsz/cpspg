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

let expr =
  let open CalcParser in
  let rec pp f = function
    | Add (l, r) -> Format.fprintf f "Add (%a, %a)" pp l pp r
    | Sub (l, r) -> Format.fprintf f "Sub (%a, %a)" pp l pp r
    | Mul (l, r) -> Format.fprintf f "Mul (%a, %a)" pp l pp r
    | Div (l, r) -> Format.fprintf f "Div (%a, %a)" pp l pp r
    | Mod (l, r) -> Format.fprintf f "Mod (%a, %a)" pp l pp r
    | Pow (l, r) -> Format.fprintf f "Pow (%a, %a)" pp l pp r
    | Neg x -> Format.fprintf f "Neg (%a)" pp x
    | Int x -> Format.fprintf f "Int %d" x
  in
  Alcotest.testable pp ( = )
;;

let suite parse =
  let open CalcParser in
  let check name tok res =
    Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check expr) name (parse (lex tok) lexbuf) res)
  in
  [ check "basic integer" [ INT 1; EOF ] (Int 1)
  ; check "basic addition" [ INT 1; PLUS; INT 2; EOF ] (Add (Int 1, Int 2))
  ; check "basic multiplication" [ INT 1; STAR; INT 2; EOF ] (Mul (Int 1, Int 2))
  ; check "basic unary minus" [ MINUS; INT 1; EOF ] (Neg (Int 1))
  ; check "double unary minus" [ MINUS; MINUS; INT 1; EOF ] (Neg (Neg (Int 1)))
  ; check
      "addition left-associativity"
      [ INT 1; PLUS; INT 2; PLUS; INT 3; EOF ]
      (Add (Add (Int 1, Int 2), Int 3))
  ; check
      "addition and substraction left-associativity"
      [ INT 1; PLUS; INT 2; MINUS; INT 3; EOF ]
      (Sub (Add (Int 1, Int 2), Int 3))
  ; check
      "multiplication left-associativity"
      [ INT 1; STAR; INT 2; STAR; INT 3; EOF ]
      (Mul (Mul (Int 1, Int 2), Int 3))
  ; check
      "exponentation right-associativity"
      [ INT 1; CARET; INT 2; CARET; INT 3; EOF ]
      (Pow (Int 1, Pow (Int 2, Int 3)))
  ; check
      "multiplication has higher precedence than addition"
      [ INT 1; PLUS; INT 2; MINUS; INT 3; EOF ]
      (Sub (Add (Int 1, Int 2), Int 3))
  ; check
      "multiplication has higher precedence than addition"
      [ INT 1; STAR; INT 2; PLUS; INT 3; EOF ]
      (Add (Mul (Int 1, Int 2), Int 3))
  ; check
      "unary minus has lower precedence than exponentation"
      [ MINUS; INT 1; CARET; MINUS; INT 2; EOF ]
      (Neg (Pow (Int 1, Neg (Int 2))))
  ; check
      "parentheses override precedence"
      [ LPAREN; INT 1; PLUS; INT 2; RPAREN; STAR; INT 3; EOF ]
      (Mul (Add (Int 1, Int 2), Int 3))
  ; check
      "complex expression"
      [ INT 1; PLUS; INT 2; STAR; INT 3; MINUS; INT 4; SLASH; INT 2; EOF ]
      (Sub (Add (Int 1, Mul (Int 2, Int 3)), Div (Int 4, Int 2)))
  ; check
      "nested parentheses"
      [ LPAREN; INT 1; PLUS; LPAREN; INT 2; STAR; INT 3; RPAREN; RPAREN; EOF ]
      (Add (Int 1, Mul (Int 2, Int 3)))
  ]
;;

let _ =
  Alcotest.run
    "Calc"
    [ "base", suite CalcParser.main_base; "prec", suite CalcParser.main_prec ]
;;
