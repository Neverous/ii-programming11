% niszczy kolejność
%select_min([H|T], M, R) :-
%	select_min(T, M, R, H).
%
%select_min([], M, [], M).
%select_min([H|T], M, [A|R], A) :-
%	H < A, !,
%	select_min(T, M, R, H).
%
%select_min([H|T], M, [H|R], A) :-
%	select_min(T, M, R, A).
%
	
select_min([H|T], M, R) :-
	select_min_only(T, M, H),
	select(M, [H|T], R).

select_min_only([], M, M).
select_min_only([H|T], M, A) :-
	H < A, !,
	select_min_only(T, M, H).

select_min_only([_|T], M, A) :-
	select_min_only(T, M, A).

sel_sort([], []).
sel_sort(L, [M|T]) :-
	select_min(L, M, R),
	sel_sort(R, T).
