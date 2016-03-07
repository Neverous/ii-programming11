sublist(_, []).
sublist([A|T], [A|T2]) :-
	sublist(T, T2).

sublist([_|T2], A) :-
	sublist(T2, A).
