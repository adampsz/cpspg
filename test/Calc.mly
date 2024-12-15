%{
  type expr =
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Mod of expr * expr
    | Pow of expr * expr
    | Neg of expr
    | Int of int
%}

%token<int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%start<int> main_base main_prec

%left PLUS MINUS
%left SLASH STAR PERCENT
%nonassoc UMINUS
%right CARET

%%

main_base: x=expr EOF { x };

expr:
  | l=expr PLUS  r=term { Add (l, r) }
  | l=expr MINUS r=term { Sub (l, r) }
  | x=term              { x }
;

term:
  | l=term STAR    r=factor { Mul (l, r) }
  | l=term SLASH   r=factor { Div (l, r) }
  | l=term PERCENT r=factor { Mod (l, r) }
  | x=factor                { x }
;

factor:
  | MINUS x=factor        { Neg x }
  | l=base CARET r=factor { Pow (l, r) }
  | x=base                { x }
;

base:
  | x=INT                { Int x }
  | LPAREN x=expr RPAREN { x }
;

main_prec: x=exprp EOF { x };

exprp:
    | l=exprp PLUS    r=exprp { Add (l, r) }
    | l=exprp MINUS   r=exprp { Sub (l, r) }
    | l=exprp STAR    r=exprp { Mul (l, r) }
    | l=exprp SLASH   r=exprp { Div (l, r) }
    | l=exprp PERCENT r=exprp { Mod (l, r) }
    | l=exprp CARET   r=exprp { Pow (l, r) }

    | MINUS x=exprp %prec UMINUS { Neg x }

    | LPAREN x=exprp RPAREN  { x }
    | x=INT                  { Int x }
;
