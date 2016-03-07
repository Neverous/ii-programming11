c_n(D, N) :-
	c_n(D, N, 0).

c_n([], A, A).
c_n([H|T], R, A) :-
	B is A * 10 + H,
	c_n(T, R, B).
