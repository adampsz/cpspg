%token COMMA EOF BAR
%token<int> NUM

%start<int list> numbers numbers_comma numbers_comma_nonempty numbers_bar

%%

numbers:                xs=repeated(NUM) EOF                  { xs };
numbers_comma:          xs=separated(NUM, COMMA) EOF          { xs };
numbers_comma_nonempty: xs=separated_nonempty(NUM, COMMA) EOF { xs };
numbers_bar:            xs=between_bars(repeated, NUM) EOF    { xs };

repeated(x):
    |                    { [] }
    | x=x xs=repeated(x) { x :: xs }
;

separated(x, sep):
    |                              { [] }
    | x=x                          { [x] }
    | x=x sep xs=separated(x, sep) { x :: xs }
;

separated_nonempty(x, sep):
    | x=x                                   { [x] }
    | x=x sep                               { [x] }
    | x=x sep xs=separated_nonempty(x, sep) { x :: xs }
;

between_bars(list, x):
    | BAR xs=list(x) BAR { xs }
;
