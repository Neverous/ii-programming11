permutation(X, Y) :-
	permutation(X, Y, X).

permutation([], [], []).
permutation(X, [A|R], [_|Z]) :-
	permutation(Y, R, Z), select(A, X, Y).
