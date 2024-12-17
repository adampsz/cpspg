%token A B SEP EOF
%start<bool list> main

%%

main:
  | a=flip SEP b=flip c=flip d=flip EOF { [a; b; c; d] }
;

%inline flip:
  | A { true }
  | B { false }
;
