%{

open Ast

let mknode ~loc data = { loc; data }

let (plus, star, qmark) =
    let loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
    let sym name = NTerm (mknode ~loc name) in
    (sym "nonempty_list", sym "list", sym "option")

%}

%start<Grammar.t> grammar

%token<string> ID TID TYPE
%token<code> CODE
%token<string> DCODE
%token DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP
%token DPREC
%token BAR COLON COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN
%token EOF

%%

grammar:
    | decls=decl* DSEP rules=rule* EOF { { decls; rules } }
;

decl:
    | code=DCODE               { DeclCode (mknode ~loc:$loc code) }
    | DTOKEN tp=tp? xs=tid*    { DeclToken (tp, xs) }
    | DSTART tp=tp? xs=id*     { DeclStart (tp, xs) }
    | DTYPE  tp=tp  xs=symbol* { DeclType (tp, xs) }
    | DLEFT         xs=symbol* { DeclLeft xs }
    | DRIGHT        xs=symbol* { DeclRight xs }
    | DNONASSOC     xs=symbol* { DeclNonassoc xs }
;

rule:
    | id=id params=rule_parameters COLON
      prods=rule_prods SEMI { { id; params; prods } }
;

rule_parameters:
    |                                          { [] }
    | LPAREN params=rule_parameter_list RPAREN { params }
;

rule_parameter_list:
    |                                       { [] }
    | x=symbol                              { [x] }
    | x=symbol COMMA xs=rule_parameter_list { x :: xs }
;

rule_prods:
    | xs=productions              { xs }
    | x=production xs=productions { x :: xs }
;

productions:
    | (* empty *)                     { [] }
    | BAR x=production xs=productions { x :: xs }
;

production:
    | prod=producer*
      prec=prec?
      action=code
    { { prod; prec; action } }
;

producer:
    | id=id EQ actual=actual { { id = Some id; actual } }
    | actual=actual          { { id = None; actual } }
;

actual:
    | actual=actual PLUS                           { { symbol = plus;  args = [Arg actual] } }
    | actual=actual STAR                           { { symbol = star;  args = [Arg actual] } }
    | actual=actual QMARK                          { { symbol = qmark; args = [Arg actual] } }
    | symbol=symbol                                { { symbol; args = [] } }
    | symbol=symbol LPAREN args=actual_args RPAREN { { symbol; args } }
;

actual_args:
    |                               { [] }
    | x=actual                      { [Arg x] }
    | x=actual COMMA xs=actual_args { Arg x :: xs }
;

symbol:
    | name=id  { NTerm name }
    | name=tid { Term name }
;

prec: DPREC x=symbol { x };

id:    x=ID   { mknode ~loc:$loc x };
tid:   x=TID  { mknode ~loc:$loc x };
tp:    x=TYPE { mknode ~loc:$loc x };
code:  x=CODE { mknode ~loc:$loc x };
