let rec gen_input depth f =
  let gen = gen_input (depth - 1) in
  match Random.int 10 with
  | _ when depth = 0 -> Format.fprintf f "%d" (Random.int 100)
  | 0 -> Format.fprintf f "%t + %t" gen gen
  | 1 -> Format.fprintf f "%t - %t" gen gen
  | 2 -> Format.fprintf f "%t * %t" gen gen
  | 3 -> Format.fprintf f "%t / %t" gen gen
  | 4 -> Format.fprintf f "%t %% %t" gen gen
  | 5 -> Format.fprintf f "%t ^ %t" gen gen
  | 6 -> Format.fprintf f "-%t" gen
  | _ -> Format.fprintf f "(%t)" gen
;;

let gen_input depth = Format.asprintf "%t" (gen_input depth)

let rec read_to_end lexfun lexbuf acc =
  match lexfun lexbuf with
  | Parser.EOF -> List.rev (Parser.EOF :: acc)
  | tok -> read_to_end lexfun lexbuf (tok :: acc)
;;

let read_to_end lexfun lexbuf = read_to_end lexfun lexbuf []

let lexfun_of_list xs =
  let xs = ref xs in
  fun _ ->
    match !xs with
    | [] -> Parser.EOF
    | x :: tail ->
      xs := tail;
      x
;;

let _ =
  Random.init (Unix.gettimeofday () |> int_of_float);
  let input = gen_input 25 in
  let lexbuf = Lexing.from_string input in
  let tokens = read_to_end Lexer.token lexbuf in
  for i = 1 to 100 do
    Parser.main_dummy (lexfun_of_list tokens) lexbuf |> ignore
  done
;;
