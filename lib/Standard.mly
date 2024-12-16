%%

option(X):
  |     { None }
  | x=X { Some x }
;

boption(X):
  |   { false }
  | X { true }
;

loption(X):
  |     { [] }
  | x=X { x }
;

list(X):
  |                { [] }
  | x=X xs=list(X) { x :: xs }
;

nonempty_list(X):
  | x=X                     { [ x ] }
  | x=X xs=nonempty_list(X) { x :: xs }
;

separated_nonempty_list(separator, X):
  | x=X                                                    { [ x ] }
  | x=X separator xs=separated_nonempty_list(separator, X) { x :: xs }
;
