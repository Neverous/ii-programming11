rength(X, Y) :-
	rength(X, Y, 0).

rength([], R, R).
rength([_|T], R, A) :-
	var(R), !,
	B is A + 1,
	rength(T, R, B).

rength([_|T], R, A) :-
	R > A,
	B is A + 1,
	rength(T, R, B).

rength2(X, Y) :-
	rength2(X, Y, 0).

rength2([], R, R).
rength2([_|T], R, A) :-
	R \== A,
	B is A + 1,
	rength2(T, R, B).
