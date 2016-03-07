halve(List, Left, Right) :-
	halve(List, Left, Right, List).

halve(List, [], List, []).
halve(List, [], List, [_]).
halve([Head|Tail], [Head|Left], Right, [_, _|Watchdog]) :-
	halve(Tail, Left, Right, Watchdog).

