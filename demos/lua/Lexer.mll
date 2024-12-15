{
  open Parser
  open Lexing

  let name_or_kw = function
    | "and" -> KW_AND
    | "break" -> KW_BREAK
    | "do" -> KW_DO
    | "else" -> KW_ELSE
    | "elseif" -> KW_ELSEIF
    | "end" -> KW_END
    | "false" -> KW_FALSE
    | "for" -> KW_FOR
    | "function" -> KW_FUNCTION
    | "goto" -> KW_GOTO
    | "if" -> KW_IF
    | "in" -> KW_IN
    | "local" -> KW_LOCAL
    | "nil" -> KW_NIL
    | "not" -> KW_NOT
    | "or" -> KW_OR
    | "repeat" -> KW_REPEAT
    | "return" -> KW_RETURN
    | "then" -> KW_THEN
    | "true" -> KW_TRUE
    | "until" -> KW_UNTIL
    | "while" -> KW_WHILE
    | id -> NAME id
  ;;

  let escape = function
    | 'a' -> '\x07'
    | 'b' -> '\b'
    | 'f' -> '\x0c'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'v' -> '\x0b'
    | '\\' -> '\\'
    | '\"' -> '\"'
    | '\'' -> '\''
    | '\n' -> '\n'
    | _ -> assert false
  ;;

  let buffered lexbuf f =
    let pos = lexbuf.lex_start_p
    and buf = Buffer.create 64 in
    f buf lexbuf;
    lexbuf.lex_start_p <- pos;
    Buffer.contents buf
  ;;
}

let space =  [' ' '\t' '\r' '\x0b']
let name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digits = ['0'-'9']+
let hexdigits = ['0'-'9' 'a'-'f' 'A'-'F']+

rule token = parse
  | eof    { EOF }
  | space+ { token lexbuf }
  | '\n'   { new_line lexbuf; token lexbuf }

  | "--" { comment lexbuf; token lexbuf }
  | "--" '[' ('='* as l) '[' { let _ = buffered lexbuf (longstring (String.length l) false) in token lexbuf }

  | name as n  { name_or_kw n }

  | "+"   { P_PLUS }
  | "-"   { P_MINUS }
  | "*"   { P_STAR }
  | "/"   { P_SLASH }
  | "%"   { P_PERCENT }
  | "^"   { P_HAT }
  | "#"   { P_LENGTH }
  | "~"   { P_TILDE }
  | "&"   { P_AND }
  | "|"   { P_OR }
  | "<<"  { P_SHL }
  | ">>"  { P_SHR }
  | "//"  { P_IDIV }
  | "=="  { P_EQ }
  | "~="  { P_NE }
  | "<="  { P_LE }
  | ">="  { P_GE }
  | "<"   { P_LT }
  | ">"   { P_GT }
  | "="   { P_ASSIGN }
  | "("   { P_LPAREN }
  | ")"   { P_RPAREN }
  | "{"   { P_LBRACE }
  | "}"   { P_RBRACE }
  | "["   { P_LBRACKET }
  | "]"   { P_RBRACKET }
  | "::"  { P_DBCOLON }
  | ";"   { P_SEMI }
  | ":"   { P_COLON }
  | ","   { P_COMMA }
  | "."   { P_DOT }
  | ".."  { P_CONCAT }
  | "..." { P_DOTS }

  | digits as n { INT n }
  | '0' ['x' 'X'] hexdigits as n { INT n }

  | digits? ('.' digits?) (['e' 'E'] ['+' '-']? digits)?
  | digits? ('.' digits?)? (['e' 'E'] ['+' '-']? digits)
  | '0' ['x' 'X'] hexdigits? ('.' hexdigits?) (['p' 'P'] ['+' '-']? hexdigits)?
  | '0' ['x' 'X'] hexdigits? ('.' hexdigits?)? (['p' 'P'] ['+' '-']? hexdigits) as n
    { NUMERAL n }

  | "\""  { STRING (buffered lexbuf (string '\"')) }
  | "\'"  { STRING (buffered lexbuf (string '\'')) }

  | '[' ('='* as l) '[' { STRING (buffered lexbuf (longstring (String.length l) false)) }

and string e buf = parse
  | "\\" (['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '\"' '\''] as c)
    { Buffer.add_char buf (escape c); string e buf lexbuf }
  | "\\" ('\n' as c)
    { Buffer.add_char buf (escape c); new_line lexbuf; string e buf lexbuf }

  | "\\z" { string_space lexbuf; string e buf lexbuf }

  | "\"" as c { if c <> e then (Buffer.add_char buf c; string e buf lexbuf) }
  | "\'" as c { if c <> e then (Buffer.add_char buf c; string e buf lexbuf) }
  | '\n' as c { Buffer.add_char buf c; new_line lexbuf; string e buf lexbuf }

  | _ as c { Buffer.add_char buf c; string e buf lexbuf }
  | eof    { failwith "unterminated string" }

and longstring l b buf = parse
  | '='* ']' as c
    { if b && String.length c - 1 = l then ()
      else (
        if b then Buffer.add_char buf ']';
        Buffer.add_string buf c;
        longstring l true buf lexbuf) }

  | '\n'   { Buffer.add_char buf '\n'; new_line lexbuf; longstring l false buf lexbuf }
  | _ as c { Buffer.add_char buf c; longstring l false buf lexbuf }
  | eof    { failwith "unterminated string" }

and string_space = parse
  | space* '\n' { new_line lexbuf; string_space lexbuf }
  | space*      { }

and comment = parse
  | '\n' { new_line lexbuf }
  | _    { comment lexbuf }
  | eof  { failwith "unterminated comment" }
