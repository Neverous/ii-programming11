bin([0]).
bin([1|T]) :-
	almostbin(T, T, []).

almostbin(T, [], T).
almostbin(X, [_|T], A) :-
	almostbin(X, T, [F|A]),
	bin([F]).

rbin([0]).
rbin([1]).
rbin([H|T]) :-
	rbin(T),
	T \= [0],
	rbin([H]).
