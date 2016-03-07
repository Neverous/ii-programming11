filter([], []).
filter([H|T], [H|R]) :-
	H > 0,
	!,
	filter(T, R).

filter([_|T], R) :- % H < 0 bo ! wyżej
	filter(T, R).
