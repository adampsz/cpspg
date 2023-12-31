{
  open Parser
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+' { PLUS }
  | '-' { MINUS}
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '^' { CARET }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _   { failwith "error" }
