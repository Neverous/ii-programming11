digit(0).
digit(1).

rbin([0]).
rbin(X) :- rbinaux(X).
rbinaux([1]).
rbinaux([D|R]) :-
	rbinaux(R),
	digit(D).

bin([0]).
bin([1|R]) :- binaux([], R).
binaux(X, X).
binaux(X, Y) :-
	binaux([D|X], Y),
	digit(D).
