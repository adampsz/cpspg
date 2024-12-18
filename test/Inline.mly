%token A B SEP EOF
%start<bool list> main

%%

main:
  | a=flip
    SEP
    b=endrule(x=flop { not x })
    c=endrule(SEP flip { $2 })
    d=midrule(flop { not $1 })
    EOF
      { [a; b; c; d] }
;

%inline flip:
  | A { true }
  | B { false }
;

%inline flop:
  | x=flip { not x }
;
