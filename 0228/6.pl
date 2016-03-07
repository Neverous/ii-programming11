reverse(X, Y) :-
	reverse(X, Y, [], Y).

reverse([], A, A, []).
reverse([H|T], A, Y, [_|Z]) :-
	reverse(T, A, [H|Y], Z).
