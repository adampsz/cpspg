{
  open Parser
  open Grammar
  open Lexing

  let add_c = Buffer.add_char
  let add_s = Buffer.add_string

  let update_loc lexbuf file line  =
    let pos = lexbuf.lex_curr_p in
    let file = Option.value ~default:pos.pos_fname file in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = file;
      pos_lnum = line;
      pos_bol = pos.pos_cnum;
    }

  let buffered pre f lexbuf =
    let pos = lexbuf.lex_start_p in
    let buf = Buffer.create 64 in
    add_s buf pre;
    let res = f buf lexbuf in
    lexbuf.lex_start_p <- pos;
    res

  let keyword_of_string = function
    | "$startpos" -> KwStartpos
    | "$endpos" -> KwEndpos
    | "$symbolstartpos" -> KwSymbolstartpos
    | "$startofs" -> KwStartofs
    | "$endofs" -> KwEndofs
    | "$symbolstartofs" -> KwSymbolstartofs
    | "$loc" -> KwLoc
    | "$sloc" -> KwSloc
    | _ -> assert false
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
  | "%prec"     { DPREC }
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

  | '<'  { TYPE (buffered " "  (tag 0) lexbuf) }
  | "{"  { CODE (buffered " "  (code "}" 0 []) lexbuf) }
  | "%{" { CODE (buffered "  " (code "%}" 0 []) lexbuf) }

  | eof { EOF }

and tag depth buf = parse
  | ['[' '('] as c { add_c buf c; tag (depth + 1) buf lexbuf }
  | [']' ')'] as c { add_c buf c; tag (depth - 1) buf lexbuf }

  | "->" as c { add_s buf c; tag depth buf lexbuf }

  | '>' as c  {
    if depth > 0
    then (add_c buf c; tag depth buf lexbuf)
    else (add_c buf ' '; Buffer.contents buf) }
  
  | newline as c { new_line lexbuf; add_s buf c; tag depth buf lexbuf }
  | eof          { failwith "unterminated type tag" }
  | _ as c       { add_c buf c; tag depth buf lexbuf }

and code e depth kw buf = parse
  | ['[' '(' '{'] as c { add_c buf c; code e (depth + 1) kw buf lexbuf }
  | [']' ')'] as c { add_c buf c; code e (depth - 1) kw buf lexbuf }

  | "}" | "%}" as c
    { if depth > 0 || c <> e
      then (add_s buf c; code e (depth - 1) kw buf lexbuf)
      else (add_s buf (String.make (String.length c) ' '); Buffer.contents buf, kw) }

  | "$startpos"
  | "$endpos"
  | "$symbolstartpos"
  | "$startofs"
  | "$endofs"
  | "$symbolstartofs"
  | "$loc"
  | "$sloc" as k
    { 
      add_s buf k;
      let k = keyword_of_string k, (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      code e depth (k :: kw) buf lexbuf }

  | '"' as c { add_c buf c; string buf lexbuf |> ignore; add_c buf c; code e depth kw buf lexbuf }

  | newline as c { new_line lexbuf; add_s buf c; code e depth kw buf lexbuf }
  | eof          { failwith "unterminated code" }
  | _ as c       { add_c buf c; code e depth kw buf lexbuf }

and string buf = parse
  | "\\\\" as c { add_s buf c; string buf lexbuf }
  | "\\\"" as c { add_s buf c; string buf lexbuf }
  | _ as c      { add_c buf c; string buf lexbuf }
  | '"' { Buffer.contents buf }

and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }

  | "\"" { string (Buffer.create 0) lexbuf |> ignore; comment depth lexbuf }

  | newline { new_line lexbuf; comment depth lexbuf }
  | eof     { failwith "unterminated comment" }
  | _       { comment depth lexbuf }
