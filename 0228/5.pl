insert([], X, [X]).
insert([H|T], E, [H|Q]) :-
	H < E, !,
	insert(T, E, Q).

insert([H|T], E, [E, H|T]).

ins_sort(L, S) :-
	ins_sort(L, S, []).

ins_sort([], X, X).
ins_sort([H|T], R, A) :-
	insert(A, H, A2),
	ins_sort(T, R, A2).

