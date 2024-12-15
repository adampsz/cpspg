let _ =
  let open ParamParser in
  let ints = Alcotest.list Alcotest.int in
  let check = Common.check ints numbers
  and check_comma = Common.check ints numbers_comma
  and check_comma_nonempty = Common.check ints numbers_comma_nonempty
  and check_bar = Common.check ints numbers_bar in
  Alcotest.run
    "param"
    [ ( "numbers"
      , [ check "empty" [ EOF ] []
        ; check "single" [ NUM 1; EOF ] [ 1 ]
        ; check "double" [ NUM 1; NUM 2; EOF ] [ 1; 2 ]
        ; check "triple" [ NUM 1; NUM 2; NUM 3; EOF ] [ 1; 2; 3 ]
        ] )
    ; ( "numbers_comma"
      , [ check_comma "empty" [ EOF ] []
        ; check_comma "single" [ NUM 1; EOF ] [ 1 ]
        ; check_comma "single trailing" [ NUM 1; COMMA; EOF ] [ 1 ]
        ; check_comma "double" [ NUM 1; COMMA; NUM 2; EOF ] [ 1; 2 ]
        ; check_comma "double trailing" [ NUM 1; COMMA; NUM 2; COMMA; EOF ] [ 1; 2 ]
        ] )
    ; ( "numbers_comma_nonempty"
      , [ check_comma_nonempty "single" [ NUM 1; EOF ] [ 1 ]
        ; check_comma_nonempty "single trailing" [ NUM 1; COMMA; EOF ] [ 1 ]
        ; check_comma_nonempty "double" [ NUM 1; COMMA; NUM 2; EOF ] [ 1; 2 ]
        ; check_comma_nonempty
            "double trailing"
            [ NUM 1; COMMA; NUM 2; COMMA; EOF ]
            [ 1; 2 ]
        ] )
    ; ( "numbers_bar"
      , [ check_bar "empty" [ BAR; BAR; EOF ] []
        ; check_bar "single" [ BAR; NUM 1; BAR; EOF ] [ 1 ]
        ; check_bar "double" [ BAR; NUM 1; NUM 2; BAR; EOF ] [ 1; 2 ]
        ; check_bar "triple" [ BAR; NUM 1; NUM 2; NUM 3; BAR; EOF ] [ 1; 2; 3 ]
        ] )
    ]
;;
