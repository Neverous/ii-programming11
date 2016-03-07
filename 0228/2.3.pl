exp(_, 0, 1).
%exp(A, P, R) :-
%	P < 0, !,
%	Q is -P,
%	exp(A, Q, W),
%	R is 1 / W.
exp(A, P, R) :-
	P > 0,% !,
	L is P - 1,
	exp(A, L, S),
	R is A * S.
