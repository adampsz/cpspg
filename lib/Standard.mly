%%

(* [midrule] and [endrule] *)

%inline
endrule(X): x=X { x };

midrule(X): x=X { x };

(* Options *)

option(X):
  |     { None }
  | x=X { Some x }
;

%inline
ioption(X):
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

(* Sequences *)

%inline
pair(X, Y):
  | x=X y=Y { (x, y) }
;

%inline
separated_pair(X, sep, Y):
  | x=X sep y=Y { (x, y) }
;

%inline
preceded(opening, X):
  | opening x=X { x }
;

%inline
terminated(X, closing):
  | x=X closing { x }
;

%inline
delimited(opening, X, closing):
  | opening x=X closing { x }
;

(* Lists *)

list(X):
  |                { [] }
  | x=X xs=list(X) { x :: xs }
;

nonempty_list(X):
  | x=X                     { [ x ] }
  | x=X xs=nonempty_list(X) { x :: xs }
;

%inline
separated_list(separator, X):
  | xs=loption(separated_nonempty_list(separator, X)) { xs }
;

separated_nonempty_list(separator, X):
  | x=X                                                    { [ x ] }
  | x=X separator xs=separated_nonempty_list(separator, X) { x :: xs }
;

(* List manipulation. *)

%inline
rev(XS):
  | xs=XS { List.rev xs }
;

%inline
flatten(XSS):
  | xss=XSS { List.flatten xss }
;

%inline
append(XS, YS):
  xs=XS ys=YS { xs @ ys }
;
