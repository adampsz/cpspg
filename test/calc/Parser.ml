[@@@warning "-unused-rec-flag"]

open Lexer

let rec pow a = function
| 0 -> 1
| 1 -> a
| n ->
let b = pow a (n / 2) in
b * b * (if n mod 2 = 0 then 1 else a)
;;

module Actions = struct
  let a1 r l () = l + r
  let a2 r l () = l - r
  let a3 term () = term
  let a4 r l () = l * r
  let a5 r l () = l / r
  let a6 r l () = l mod r
  let a7 factor () = factor
  let a8 r l () = pow l r
  let a9 base () = base
  let a10 expression () = expression
  let a11 x () = x
end

module States = struct
  let lexfun = ref (fun _ -> assert false)
  let lexbuf = ref (Lexing.from_string String.empty)
  let lookahead = ref None

  let setup lf lb =
    lexfun := lf;
    lexbuf := lb;
    lookahead := None
  ;;

  let shift () =
    let t = Option.get !lookahead in
    lookahead := None;
    t
  ;;

  let lookahead () =
    match !lookahead with
    | Some t -> t
    | None ->
      let t = !lexfun !lexbuf in
      lookahead := Some t;
      t
  ;;

  let rec s0 c0 =
    let rec c1 x = s20 x c0 c1
    and c2 x = s19 x c1 c2
    and c3 x = s6 x c2
    and c4 x = s3 x c3 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c4
    | Some LPAREN ->
      let _ = shift () in
      s2 c4
    | _ -> raise (Failure "error in state 0")

  and s1 a0 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some CARET | Some RPAREN ->
      let x = Actions.a11 a0 () in
      c0 x
    | _ -> raise (Failure "error in state 1")

  and s2 c0 =
    let rec c1 x = s7 x c0 c1
    and c2 x = s19 x c1 c2
    and c3 x = s6 x c2
    and c4 x = s3 x c3 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c4
    | Some LPAREN ->
      let _ = shift () in
      s2 c4
    | _ -> raise (Failure "error in state 2")

  and s3 a0 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a9 a0 () in
      c0 x
    | Some CARET ->
      let _ = shift () in
      s4 a0 c0
    | _ -> raise (Failure "error in state 3")

  and s4 a1 c0 =
    let rec c1 x = s5 x a1 c0
    and c2 x = s3 x c1 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c2
    | Some LPAREN ->
      let _ = shift () in
      s2 c2
    | _ -> raise (Failure "error in state 4")

  and s5 a0 a2 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a8 a0 a2 () in
      c0 x
    | _ -> raise (Failure "error in state 5")

  and s6 a0 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a7 a0 () in
      c0 x
    | _ -> raise (Failure "error in state 6")

  and s7 a0 c0 c1 =
    match lookahead () with
    | Some PLUS ->
      let _ = shift () in
      s8 a0 c1
    | Some MINUS ->
      let _ = shift () in
      s17 a0 c1
    | Some RPAREN ->
      let _ = shift () in
      s16 a0 c0
    | _ -> raise (Failure "error in state 7")

  and s8 a1 c0 =
    let rec c1 x = s9 x a1 c0 c1
    and c2 x = s6 x c1
    and c3 x = s3 x c2 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c3
    | Some LPAREN ->
      let _ = shift () in
      s2 c3
    | _ -> raise (Failure "error in state 8")

  and s9 a0 a2 c0 c1 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some RPAREN ->
      let x = Actions.a1 a0 a2 () in
      c0 x
    | Some SLASH ->
      let _ = shift () in
      s12 a0 c1
    | Some STAR ->
      let _ = shift () in
      s10 a0 c1
    | Some PERCENT ->
      let _ = shift () in
      s14 a0 c1
    | _ -> raise (Failure "error in state 9")

  and s10 a1 c0 =
    let rec c1 x = s11 x a1 c0
    and c2 x = s3 x c1 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c2
    | Some LPAREN ->
      let _ = shift () in
      s2 c2
    | _ -> raise (Failure "error in state 10")

  and s11 a0 a2 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a4 a0 a2 () in
      c0 x
    | _ -> raise (Failure "error in state 11")

  and s12 a1 c0 =
    let rec c1 x = s13 x a1 c0
    and c2 x = s3 x c1 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c2
    | Some LPAREN ->
      let _ = shift () in
      s2 c2
    | _ -> raise (Failure "error in state 12")

  and s13 a0 a2 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a5 a0 a2 () in
      c0 x
    | _ -> raise (Failure "error in state 13")

  and s14 a1 c0 =
    let rec c1 x = s15 x a1 c0
    and c2 x = s3 x c1 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c2
    | Some LPAREN ->
      let _ = shift () in
      s2 c2
    | _ -> raise (Failure "error in state 14")

  and s15 a0 a2 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some RPAREN ->
      let x = Actions.a6 a0 a2 () in
      c0 x
    | _ -> raise (Failure "error in state 15")

  and s16 a1 c0 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some SLASH | Some STAR | Some PERCENT | Some CARET | Some RPAREN ->
      let x = Actions.a10 a1 () in
      c0 x
    | _ -> raise (Failure "error in state 16")

  and s17 a1 c0 =
    let rec c1 x = s18 x a1 c0 c1
    and c2 x = s6 x c1
    and c3 x = s3 x c2 in
    match lookahead () with
    | Some (INT x) ->
      let _ = shift () in
      s1 x c3
    | Some LPAREN ->
      let _ = shift () in
      s2 c3
    | _ -> raise (Failure "error in state 17")

  and s18 a0 a2 c0 c1 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some RPAREN ->
      let x = Actions.a2 a0 a2 () in
      c0 x
    | Some SLASH ->
      let _ = shift () in
      s12 a0 c1
    | Some STAR ->
      let _ = shift () in
      s10 a0 c1
    | Some PERCENT ->
      let _ = shift () in
      s14 a0 c1
    | _ -> raise (Failure "error in state 18")

  and s19 a0 c0 c1 =
    match lookahead () with
    | None | Some PLUS | Some MINUS | Some RPAREN ->
      let x = Actions.a3 a0 () in
      c0 x
    | Some SLASH ->
      let _ = shift () in
      s12 a0 c1
    | Some STAR ->
      let _ = shift () in
      s10 a0 c1
    | Some PERCENT ->
      let _ = shift () in
      s14 a0 c1
    | _ -> raise (Failure "error in state 19")

  and s20 a0 c0 c1 =
    match lookahead () with
    | None ->
      let x =  a0 in
      c0 x
    | Some PLUS ->
      let _ = shift () in
      s8 a0 c1
    | Some MINUS ->
      let _ = shift () in
      s17 a0 c1
    | _ -> raise (Failure "error in state 20")

    ;;
end

let expression lexbuf lexfun =
  States.setup lexfun lexbuf;
  States.s0 (fun x -> x)
;;

