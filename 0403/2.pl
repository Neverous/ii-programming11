expression(Tree) --> simple_expression(Tree).
expression(*(Left, Right)) --> expression(Left), "*", !, simple_expression(Right).
simple_expression(a) --> "a", !.
simple_expression(b) --> "b", !.
simple_expression(Tree) --> "(", !, expression(Tree), ")".
