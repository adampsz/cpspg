%{

open Ast

let mknode ~loc data = { loc; data }

%}

%start<Grammar.t> grammar

%token<string> ID TID TYPE
%token<code> CODE
%token<string> DCODE
%token DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP
%token COLON SEMI BAR EQ
%token EOF

%%

grammar:
    | decls=decls DSEP rules=rules EOF { { decls; rules } }
;

decls:
    | (* empty *)      { [] }
    | x=decl xs=decls  { x :: xs }
;

decl:
    | code=DCODE             { DeclCode (mknode ~loc:$loc code) }
    | DTOKEN tp=tp xs=tids   { DeclToken (Some tp, xs) }
    | DTOKEN xs=tids         { DeclToken (None, xs) }
    | DSTART tp=tp xs=ids    { DeclStart (Some tp, xs) }
    | DSTART xs=ids          { DeclStart (None, xs) }
    | DTYPE tp=tp xs=symbols { DeclType (tp, xs) }
    | DLEFT xs=symbols       { DeclLeft xs }
    | DRIGHT xs=symbols      { DeclRight xs }
    | DNONASSOC xs=symbols   { DeclNonassoc xs }
;

rules:
    | (* empty *)      { [] }
    | x=rule xs=rules  { x :: xs }
;

rule:
    | id=id COLON prods=rule_prods SEMI { { id; prods } }
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
    | prod=producers prec=production_prec action=code { { prod; prec; action } }
;

production_prec:
    | (* empty *)    { None }
    | DPREC x=symbol { Some x }
;

producers:
    | (* empty *)             { [] }
    | x=producer xs=producers { x :: xs }
;

producer:
    | id=id EQ actual=symbol { { id = Some id; actual } }
    | actual=symbol          { { id = None; actual } }
;

ids:
    | (* empty *) { [] }
    | x=id xs=ids { x :: xs }
;

tids:
    | (* empty *)   { [] }
    | x=tid xs=tids { x :: xs }
;

symbols:
    | (* empty *)         { [] }
    | x=symbol xs=symbols { x :: xs }
;

symbol:
    | name=id  { NTerm name }
    | name=tid { Term name }
;

id:    x=ID   { mknode ~loc:$loc x };
tid:   x=TID  { mknode ~loc:$loc x };
tp:    x=TYPE { mknode ~loc:$loc x };
code:  x=CODE { mknode ~loc:$loc x };
