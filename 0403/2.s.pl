simple_expression(a) --> "a", !.
simple_expression(b) --> "b", !.
simple_expression(E) --> "(", expression(E, A), ")".
expression(D, A) --> simple_expression(D), { \+ Var(A) }, !.
expression(mul(A, D), A) --> simple_expression(D).
expression(E, D) --> simple_expression(D), "*", { \+ Var(A) }, !, expression(E, D).
expression(E, A) --> simple_expression(D), "*", {new_A is mul(A, D) }, expression(E, New_A).
