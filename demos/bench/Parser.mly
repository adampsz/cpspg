%{

(* Used to disable optimizations inside semantic actions
   TODO: is there a proper way to do it? *)
external blackbox: int -> int = "%identity"
let blackbox2 a b = blackbox a lxor blackbox b

%}

%token<int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%type<int> math
%type<unit> dyck leftrec rightrec
%type<int> expr
%type<unit> parens left right
%start math dyck leftrec rightrec

%left PLUS MINUS
%left SLASH STAR PERCENT
%nonassoc UMINUS
%right CARET

%%

math: expr EOF { blackbox $1 };

expr:
    | expr PLUS    expr { blackbox2 $1 $3 }
    | expr MINUS   expr { blackbox2 $1 $3 }
    | expr STAR    expr { blackbox2 $1 $3 }
    | expr SLASH   expr { blackbox2 $1 $3 }
    | expr PERCENT expr { blackbox2 $1 $3 }
    | expr CARET   expr { blackbox2 $1 $3 }

    | MINUS expr %prec UMINUS { blackbox $2 }
    
    | LPAREN expr RPAREN  { blackbox $2 }
    | INT                 { blackbox $1 }
;

dyck: parens EOF { () };

parens:
    |                             { () }
    | LPAREN parens RPAREN parens { () }
;

leftrec:  left  EOF { () };
rightrec: right EOF { () };

left:
    |             { () }
    | left PLUS   { () }
;

right:
    |             { () }
    | PLUS right  { () }
;
