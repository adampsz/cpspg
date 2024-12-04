{

open Ast
open Parser
open Lexing

let add_c = Buffer.add_char
let add_s = Buffer.add_string

let update_loc lexbuf file line =
  let pos = lexbuf.lex_curr_p in
  let file = Option.value ~default:pos.pos_fname file in
  lexbuf.lex_curr_p
    <- { pos with pos_fname = file; pos_lnum = line; pos_bol = pos.pos_cnum }
;;

let sync buf lexbuf = Lexing.lexeme lexbuf |> Buffer.add_string buf

let wrapped pre post f lexbuf =
  let buf = Buffer.create 64
  and pos = lexbuf.lex_start_p in
  Buffer.add_string buf pre;
  let res = f (sync buf) lexbuf in
  Buffer.add_string buf post;
  lexbuf.lex_start_p <- pos;
  Buffer.contents buf, res
;;

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
;;

}

let newline = '\r'* '\n'
let blank = [' ' '\009' '\012']

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule main = parse
  | newline { new_line lexbuf; main lexbuf }
  | blank   { main lexbuf }

  | "#" blank* (['0'-'9']+ as num) blank*
      ('"' ([^ '\r' '\n' '"']* as name) '"')?
      [^ '\r' '\n']* newline
    { update_loc lexbuf name (int_of_string num); main lexbuf }

  | "(*" { wrapped "" "" (comment 0) lexbuf |> ignore; main lexbuf }

  | "/*" { ccomment lexbuf; main lexbuf }
  | "//" { ccomment_line lexbuf; main lexbuf }

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
  | "%{"        { DCODE (wrapped "  " "  " (dcode 0) lexbuf |> fst) }

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

  | '<'  { TYPE (wrapped " " " " (tag 0) lexbuf |> fst) }
  | "{"  { CODE (wrapped " " " " (code 0 []) lexbuf) }

  | eof { EOF }

and tag depth eat = parse
  | '[' | '(' { eat lexbuf; tag (depth + 1) eat lexbuf }
  | ']' | ')' { eat lexbuf; tag (depth - 1) eat lexbuf }

  | "->" { eat lexbuf; tag depth eat lexbuf }
  | '>'  { if depth > 0 then (eat lexbuf; tag depth eat lexbuf) }

  | newline { new_line lexbuf; eat lexbuf; tag depth eat lexbuf }
  | eof     { failwith "unterminated type tag" }
  | _       { eat lexbuf; tag depth eat lexbuf }

and code depth kw eat = parse
  | '[' | '(' | '{' { eat lexbuf; code (depth + 1) kw eat lexbuf }
  | ']' | ')'       { eat lexbuf; code (depth - 1) kw eat lexbuf }
  | "}"             { if depth > 0 then (eat lexbuf; code (depth - 1) kw eat lexbuf) else kw }

  | "$startpos"
  | "$endpos"
  | "$symbolstartpos"
  | "$startofs"
  | "$endofs"
  | "$symbolstartofs"
  | "$loc"
  | "$sloc" as k
    { let k = keyword_of_string k, (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      eat lexbuf;
      code depth (k :: kw) eat lexbuf }

  | '$' (['0'-'9']+ as i)
    { let k = KwArg (int_of_string i), (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      eat lexbuf;
      code depth (k :: kw) eat lexbuf }

  | '"'  { eat lexbuf; string eat lexbuf;    eat lexbuf; code depth kw eat lexbuf }
  | "(*" { eat lexbuf; comment 0 eat lexbuf; eat lexbuf; code depth kw eat lexbuf }

  | newline { new_line lexbuf; eat lexbuf; code depth kw eat lexbuf }
  | eof     { failwith "unterminated code" }
  | _       { eat lexbuf; code depth kw eat lexbuf }

and dcode depth eat = parse
  | '[' | '(' | '{' { eat lexbuf; dcode (depth + 1) eat lexbuf }
  | ']' | ')' | '}' { eat lexbuf; dcode (depth - 1) eat lexbuf }
  | "%}"            { if depth > 0 then (eat lexbuf; dcode (depth - 1) eat lexbuf) }

  | '"'  { eat lexbuf; string eat lexbuf;    eat lexbuf; dcode depth eat lexbuf }
  | "(*" { eat lexbuf; comment 0 eat lexbuf; eat lexbuf; dcode depth eat lexbuf }

  | newline { new_line lexbuf; eat lexbuf; dcode depth eat lexbuf }
  | eof     { failwith "unterminated code" }
  | _       { eat lexbuf; dcode depth eat lexbuf }

and string eat = parse
  | "\\\\"  { eat lexbuf; string eat lexbuf }
  | "\\\""  { eat lexbuf; string eat lexbuf }
  | '"'     { }
  | newline { new_line lexbuf; eat lexbuf; string eat lexbuf }
  | eof     { failwith "unterminated string" }
  | _       { eat lexbuf; string eat lexbuf }

and comment depth eat = parse
  | "(*" { eat lexbuf; comment (depth + 1) eat lexbuf }
  | "*)" { if depth > 0 then (eat lexbuf; comment (depth - 1) eat lexbuf) }

  | '"' { eat lexbuf; string eat lexbuf; eat lexbuf; comment depth eat lexbuf }

  | newline  { new_line lexbuf; eat lexbuf; comment depth eat lexbuf }
  | eof      { failwith "unterminated comment" }
  | _        { eat lexbuf; comment depth eat lexbuf }

and ccomment = parse
  | "*/" { }
  | eof  { failwith "unterminated comment" }
  | _    { ccomment lexbuf }

and ccomment_line = parse
  | "\n" { }
  | eof  { failwith "unterminated comment" }
  | _    { ccomment_line lexbuf }
