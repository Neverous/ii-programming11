sublist(_, []).
sublist([A|T], [A|T2]) :-
	sublist(T, T2).

sublist([_|T2], A) :-
	sublist(T2, A).

c_n(D, N) :-
	c_n(D, N, 0).

c_n([], A, A).
c_n([H|T], R, A) :-
	B is A * 10 + H,
	c_n(T, R, B).

query(A, C, E, P, R, S, U) :- 
	permutation([A, C, E, P, R, S, U], L), 
	sublist([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], L), 
	U \== 0, 
	P \== 0, 
	c_n([U, S, A], USA), 
	c_n([U, S, S, R], USSR), 
	c_n([P, E, A, C, E], PEACE), 
	PEACE is USA + USSR. 
