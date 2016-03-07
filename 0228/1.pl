perm(X, Y) :-
	perm(X, Y, Y).

perm([], [], []).
perm([H|T], R, [_|Z]) :-
	perm(T, W, Z), select(H, R, W).
