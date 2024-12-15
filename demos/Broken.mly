(* Broken grammar, used to show errors and warnings *)

%{

%}

%token TOK TOK
%start<unit> broken_start
%start<unit> start start

%left TOK
%left TOK

%%


start:
    | broken_nterm BROKEN_TERM { () }
    | start %prec broken_prec  { () }
    | start %prec BROKEN_PREC  { () }
    | conflict TOK             { () }
;

start:;

conflict:
    | (* reduce *)     { () }
    | TOK (* shift *)  { () }
;
