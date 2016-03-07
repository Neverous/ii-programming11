even([]).
even([X, Y|Z]) :-
	even(Z).
