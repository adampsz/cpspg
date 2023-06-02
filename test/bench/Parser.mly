%{

(* Used to disable optimizations inside semantic actions
   TODO: is there a proper way to do it? *)
external blackbox: int -> int = "%identity"
let blackbox2 a b = blackbox a lxor blackbox b

%}

%token<int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%type<int> main expr
%type<unit> main_dummy dummy
%start main main_dummy

%left PLUS MINUS
%left SLASH STAR PERCENT
%nonassoc UMINUS
%right CARET

%%

main: expr EOF { blackbox $1 };

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

main_dummy: dummy EOF { () };

dummy:
    | dummy PLUS    dummy { () }
    | dummy MINUS   dummy { () }
    | dummy STAR    dummy { () }
    | dummy SLASH   dummy { () }
    | dummy PERCENT dummy { () }
    | dummy CARET   dummy { () }

    | MINUS dummy %prec UMINUS { () }
    
    | LPAREN dummy RPAREN  { () }
    | INT                 { () }
;
