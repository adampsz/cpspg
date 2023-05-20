{
  open Parser
}

let space =  [' ' '\n' '\r' '\t' ]
let id_start = ['a'-'z' '_']
let tid_start = ['A'-'Z']
let id_cont = id_start | tid_start | ['0'-'9' ''']

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | space { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }

  | "%token" { DTOKEN }
  | "%start" { DSTART }
  | "%%" { DSEP }
  | ":" { COLON }
  | ";" { SEMI }
  | "|" { BAR }
  | "=" { EQ }

  | id_start id_cont* as i { ID i }
  | tid_start id_cont* as i { TID i }

  | '<' {
    let buf = Buffer.create 64 in
    tp buf lexbuf;
    TYPE (Buffer.contents buf)
  }

  | '{' {
    let buf = Buffer.create 64 in
    code buf lexbuf;
    CODE (Buffer.contents buf)
  }

  | eof { EOF }

and tp buf = parse
  | '<' { Buffer.add_char buf '<'; tp buf lexbuf; Buffer.add_char buf '>'; tp buf lexbuf }
  | '>' {}
  | _ as c { Buffer.add_char buf c; tp buf lexbuf }

and code buf = parse
  | '{' { Buffer.add_char buf '{'; code buf lexbuf; Buffer.add_char buf '}'; code buf lexbuf }
  | '}' {}
  | _ as c { Buffer.add_char buf c; code buf lexbuf }

and comment = parse
  | "(*" { comment lexbuf; comment lexbuf }
  | "*)" {}
  | _    { comment lexbuf }
