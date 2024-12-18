let _ =
  let open InlineParser in
  let bools = Alcotest.list Alcotest.bool in
  let check = Common.check bools main in
  Alcotest.run
    "param"
    [ ( "flip"
      , [ check "A-A-AA" [ A; SEP; A; SEP; A; A; EOF ] [ true; true; true; true ]
        ; check "A-B-AB" [ A; SEP; B; SEP; A; B; EOF ] [ true; false; true; false ]
        ; check "B-B-BB" [ B; SEP; B; SEP; B; B; EOF ] [ false; false; false; false ]
        ] )
    ]
;;
