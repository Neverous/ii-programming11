decimal(N, D) :-
	N >= 0,
	dec(N, W),
	reverse(W, D).

dec(N, [N]) :-
	N is N mod 10.

dec(N, [H|T]) :-
	H is N mod 10,
	W is (N - H) / 10,
	W > 0,
	dec(W, T).
