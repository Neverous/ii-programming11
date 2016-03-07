merge([], Sorted, Sorted).
merge(Sorted, [], Sorted).
merge([Head1|Tail1], [Head2|Tail2], [Head1|Sorted]) :-
	Head1 < Head2, !,
	merge(Tail1, [Head2|Tail2], Sorted).

merge([Head1|Tail1], [Head2|Tail2], [Head2|Sorted]) :-
	merge([Head1|Tail1], Tail2, Sorted).

halve(List, Left, Right) :-
	halve(List, Left, Right, List).

halve(List, [], List, []).
halve(List, [], List, [_]).
halve([Head|Tail], [Head|Left], Right, [_, _|Watchdog]) :-
	halve(Tail, Left, Right, Watchdog).

merge_sort([], []).
merge_sort([E], [E]).
merge_sort(List, Sorted) :-
	halve(List, Left, Right),
	merge_sort(Left, LeftSorted),
	merge_sort(Right, RightSorted),
	merge(LeftSorted, RightSorted, Sorted), !.

