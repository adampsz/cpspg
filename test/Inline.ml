let _ =
  let open InlineParser in
  let bools = Alcotest.list Alcotest.bool in
  let check = Common.check bools main in
  Alcotest.run
    "param"
    [ ( "flip"
      , [ check "A-AAA" [ A; SEP; A; A; A; EOF ] [ true; true; true; true ]
        ; check "A-BAB" [ A; SEP; B; A; B; EOF ] [ true; false; true; false ]
        ; check "B-BBB" [ B; SEP; B; B; B; EOF ] [ false; false; false; false ]
        ] )
    ]
;;
