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

%%

main: x=expr EOF { x };

expr:
    | l=expr PLUS  r=term { l + r }
    | l=expr MINUS r=term { l - r }
    | x=term                    { x }
;

term:
    | l=term STAR    r=factor { l * r }
    | l=term SLASH   r=factor { l / r }
    | l=term PERCENT r=factor { l mod r }
    | x=factor                { x }
;

factor:
    | l=base CARET r=factor { pow l r }
    | x=base                { x }
;

base:
    | x=INT                      { x }
    | LPAREN x=expr RPAREN { x }
;
