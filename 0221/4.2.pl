palindrom([], B, A, [B|A]).
palindrom([H|T], B, A, R) :-
	palindrom(T, [B|H], [H|A], R).
