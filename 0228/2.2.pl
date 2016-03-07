count(X, Y, C) :-
	count(X, Y, C, 0).

count(_, [], X, X).
count([H|X], [H|Y], C, B) :-
	A is B + 1,
	count(X, Y, C, A), !.

count(X, [_|Y], C, B) :-
	count(X, Y, C, B).
