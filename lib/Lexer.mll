{
  open Parser
  open Lexing

  let update_loc lexbuf file line  =
    let pos = lexbuf.lex_curr_p in
    let file = Option.value ~default:pos.pos_fname file in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = file;
      pos_lnum = line;
      pos_bol = pos.pos_cnum;
    }

  let buffered f lexbuf =
    let pos = lexbuf.lex_start_p in
    let buf = Buffer.create 64 in
    f buf lexbuf;
    lexbuf.lex_start_p <- pos;
    Buffer.contents buf
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule main = parse
  | newline { new_line lexbuf; main lexbuf }
  | blank   { main lexbuf }

  | "#" blank* (['0'-'9']+ as num) blank*
      ('"' ([^ '\010' '\013' '"']* as name) '"')?
      [^ '\010' '\013']* newline
    { update_loc lexbuf name (int_of_string num); main lexbuf }

  | "(*" { comment 0 lexbuf; main lexbuf }

  | "%token"    { DTOKEN}
  | "%term"     { DTOKEN }
  | "%type"     { DTYPE }
  | "%start"    { DSTART }
  | "%left"     { DLEFT }
  | "%right"    { DRIGHT }
  | "%nonassoc" { DNONASSOC }
  | "%binary"   { DNONASSOC }
  | "%%"        { DSEP }

  | "%\\" { DSEP }
  | "%<"  { DLEFT }
  | "%>"  { DRIGHT }
  | "%0"  { DTOKEN }
  | "%2"  { DNONASSOC }

  | ":" { COLON }
  | ";" { SEMI }
  | "|" { BAR }
  | "=" { EQ }

  | lowercase identchar* as i { ID i }
  | uppercase identchar* as i { TID i }

  | '<' { TYPE (buffered (tag 0) lexbuf) }
  | "{" { CODE (buffered (code false 0) lexbuf) }
  | "%{" { CODE (buffered (code true 0) lexbuf) }

  | eof { EOF }

and tag depth buf = parse
  | ['[' '('] as c { Buffer.add_char buf c; tag (depth + 1) buf lexbuf }
  | [']' ')'] as c { Buffer.add_char buf c; tag (depth - 1) buf lexbuf }

  | "->" as c { Buffer.add_string buf c; tag depth buf lexbuf }
  | '>' as c  { if depth > 0 then (Buffer.add_char buf c; tag depth buf lexbuf) }
  
  | newline as c { new_line lexbuf; Buffer.add_string buf c; tag depth buf lexbuf }
  | eof          { failwith "unterminated type tag" }
  | _ as c       { Buffer.add_char buf c; tag depth buf lexbuf }

and code head depth buf = parse
  | ['[' '(' '{'] as c { Buffer.add_char buf c; code head (depth + 1) buf lexbuf }
  | [']' ')'] as c { Buffer.add_char buf c; code head (depth - 1) buf lexbuf }

  | '}' as c  { if depth > 0 || head = true then (Buffer.add_char buf c; code head (depth - 1) buf lexbuf) }
  | "%}" as c { if depth > 0 || head = false then (Buffer.add_string buf c; code head (depth - 1) buf lexbuf) }

  (* TODO: Proper location handling *)
  | "$loc" { Buffer.add_string buf "(List.hd _loc)"; code head depth buf lexbuf }

  | '"' as c { Buffer.add_char buf c; string buf lexbuf; Buffer.add_char buf c; code head depth buf lexbuf }

  | newline as c { new_line lexbuf; Buffer.add_string buf c; code head depth buf lexbuf }
  | eof          { failwith "unterminated code" }
  | _ as c       { Buffer.add_char buf c; code head depth buf lexbuf }

and string buf = parse
  | "\\\\" as c { Buffer.add_string buf c; string buf lexbuf }
  | "\\\"" as c { Buffer.add_string buf c; string buf lexbuf }
  | _ as c      { Buffer.add_char buf c; string buf lexbuf }
  | '"' { }

and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }

  | "\"" { string (Buffer.create 0) lexbuf; comment depth lexbuf }

  | newline { new_line lexbuf; comment depth lexbuf }
  | eof     { failwith "unterminated comment" }
  | _       { comment depth lexbuf }
