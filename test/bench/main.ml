let rec gen_math depth f =
  let gen = gen_math (depth - 1) in
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

let rec gen_dyck depth f =
  let gen = gen_dyck (depth - 1) in
  if depth > 0 then Format.fprintf f "(%t)%t" gen gen
;;

let gen_math depth = Format.asprintf "%t" (gen_math depth)
let gen_dyck depth = Format.asprintf "%t" (gen_dyck depth)
let gen_plus depth = String.make (Int.shift_left 1 depth) '+'

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

let gen_lex gen depth =
  let input = gen depth in
  let lexbuf = Lexing.from_string input in
  let tokens = read_to_end Lexer.token lexbuf in
  fun () -> lexfun_of_list tokens, lexbuf
;;

let bench gen fn depth n m =
  Random.init (Unix.gettimeofday () |> int_of_float);
  let total = ref 0.0 in
  for i = 1 to n do
    let lex = gen_lex gen depth in
    let start = Unix.gettimeofday () in
    for j = 1 to m do
      let lexfun, lexbuf = lex () in
      fn lexfun lexbuf |> ignore
    done;
    let finish = Unix.gettimeofday () in
    total := !total +. (finish -. start);
    Printf.eprintf ".%!"
  done;
  Printf.printf " %fs (%fs/it)\n" !total (!total /. float_of_int (n * m))
;;

let _ =
  match Sys.argv with
  | [| _; "math" |] -> bench gen_math Parser.math 20 100 10
  | [| _; "dyck" |] -> bench gen_dyck Parser.dyck 15 100 10
  | [| _; "leftrec" |] -> bench gen_plus Parser.leftrec 15 100 10
  | [| _; "rightrec" |] -> bench gen_plus Parser.rightrec 15 100 10
  | _ -> failwith "usage: main <math|dyck|leftrec|rightrec>"
;;
