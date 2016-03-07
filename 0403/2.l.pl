simple_expression(a) --> "a", !.
simple_expression(b) --> "b", !.
simple_expression(E) --> "(", exp(E), ")"
exp(T) --> simple_expression(A), exp(A, T).
exp(A, T) --> "*", !, simple_expression(B), exp(node(A, B), T).
exp(A, T) --> {A = T}.
