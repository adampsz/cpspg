%{

open Ast

let plus, star, qmark =
  let loc = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym data = NTerm { loc; data } in
  sym "nonempty_list", sym "list", sym "option"
;;

%}

%start<Grammar.t> grammar

%token<string> ID TID TYPE
%token<code> CODE
%token<string> DCODE
%token DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP
%token DINLINE DPREC
%token BAR COLON COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN
%token EOF

%%

grammar:
  | decls=decl* DSEP rules=rule* EOF { { decls; rules } }
;

decl:
  | data=DCODE               { DeclCode { loc=$loc; data } }
  | DTOKEN tp=tp? xs=tid*    { DeclToken (tp, xs) }
  | DSTART tp=tp? xs=id*     { DeclStart (tp, xs) }
  | DTYPE  tp=tp  xs=symbol* { DeclType (tp, xs) }
  | DLEFT         xs=symbol* { DeclLeft xs }
  | DRIGHT        xs=symbol* { DeclRight xs }
  | DNONASSOC     xs=symbol* { DeclNonassoc xs }
;

rule:
  | inline=boption(DINLINE)
    id=id params=loption(parameters) COLON
    option(BAR) prods=separated_nonempty_list(BAR, production)
    SEMI
      { { id; inline; params; prods } }
;

parameters:
  | LPAREN params=separated_list(COMMA, parameter) RPAREN { params }
;

%inline
parameter:
  | x=symbol { x }
;

production:
  | prod=producer*
    prec=preceded(DPREC, symbol)?
    action=code { { prod; prec; action } }
;

producer:
  | id=ioption(terminated(id, EQ))
    actual=actual { { id; actual } }
;

actual:
  | actual=actual symbol=shorthand   { { symbol; args = [ Arg actual ] } }
  | symbol=symbol args=loption(args) { { symbol; args } }
;

shorthand:
  | PLUS  { plus }
  | STAR  { star }
  | QMARK { qmark }
;

args:
  | LPAREN args=separated_nonempty_list(COMMA, arg) RPAREN { args }
;

%inline
arg:
  | x=actual                   { Arg x }
  | prod=producer* action=code { ArgInline { prod; action } }
;

symbol:
  | name=id  { NTerm name }
  | name=tid { Term name }
;

%inline id:   node(ID)   { $1 };
%inline tid:  node(TID)  { $1 };
%inline tp:   node(TYPE) { $1 };
%inline code: node(CODE) { $1 };

%inline node(X): data=X { { loc = $loc; data } };
