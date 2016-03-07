flatten(leaf, R, R).
flatten(node(LS, V, RS), R, A) :-
	flatten(RS, R1, A), flatten(LS, R, [V|R1]).
flatten(T, R) :- flatten(T, R, []).

insert(V, leaf, node(leaf, V, leaf)).
insert(V, node(L, E, R), node(L1, E, R)) :-
	V =< E, !,
	insert(V, L, L1).

insert(V, node(L, E, R), node(L, E, R1)) :-
	insert(V, R, R1).

treesort(X, R) :-
	treesort(X, R, leaf).

treesort([], R, Tree) :-
	flatten(Tree, R).

treesort([H|T], R, Tree) :-
	insert(H, Tree, NTree),
	treesort(T, R, NTree).
