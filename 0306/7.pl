is_list([]).
is_list(X) :-
	var(X), !, fail.
is_list([_|_]).

revall(L, R) :-
	revall(L, R, []).

revall([], R, R).
revall([H|T], R, A) :-
	is_list(H), !,
	revall(H, H1),
	revall(T, R, [H1|A]).

revall([H|T], R, A) :-
	revall(T, R, [H|A]).
