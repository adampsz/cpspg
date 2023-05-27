%{

let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n -> 
        let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)
;;

%}

%token<int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%start<int> main

%left PLUS MINUS
%left SLASH STAR PERCENT
%nonassoc UMINUS
%right CARET

%%

main: x=expr EOF { x };

expr:
    | l=expr PLUS    r=expr { l + r }
    | l=expr MINUS   r=expr { l - r }
    | l=expr STAR    r=expr { l * r }
    | l=expr SLASH   r=expr { l / r }
    | l=expr PERCENT r=expr { l mod r }
    | l=expr CARET   r=expr { pow l r }

    | MINUS x=expr %prec UMINUS { -x }
    
    | LPAREN x=expr RPAREN  { x }
    | x=INT                 { x }
;
