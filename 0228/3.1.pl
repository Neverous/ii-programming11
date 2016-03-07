factorial(F, R) :-
	factorial(F, R, 1).

factorial(0, R, R).
factorial(F, R, A) :-
	F > 0,
	G is F - 1,
	B is A * F,
	factorial(G, R, B).
