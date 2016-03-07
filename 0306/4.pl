flatten(leaf, R, R).
flatten(node(LS, V, RS), R, A) :-
	flatten(RS, R1, A), flatten(LS, R, [V|R1]).
flatten(T, R) :- flatten(T, R, []).

mirror(leaf, leaf).
mirror(node(L, V, R), node(L1, V, R1)) :-
	mirror(L, R1),
	mirror(R, L1).
