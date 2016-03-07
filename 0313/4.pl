halve(X-Y, X-Z, Z-Y) :-
	halve2(X-Y, Z, X-Y), !.

halve2(Z-Y, Z, Y-Y).
halve2(Z-Y, Z, [_|Y]-Y).
halve2([_|A]-Y, Z, [_,_|B]-Y) :-
	halve2(A-Y, Z, B-Y).

merge([], Sorted, Sorted).
merge(Sorted, [], Sorted).
merge([Head1|Tail1], [Head2|Tail2], [Head1|Sorted]) :-
	Head1 < Head2, !,
	merge(Tail1, [Head2|Tail2], Sorted).

merge([Head1|Tail1], [Head2|Tail2], [Head2|Sorted]) :-
	merge([Head1|Tail1], Tail2, Sorted).

mergesort(Y-Y, []).
mergesort([A|Y]-Y, [A]).
mergesort(X-Y, Sorted) :-
	halve(X-Y, Left, Right),
	mergesort(Left, LeftSorted),
	mergesort(Right, RightSorted),
	merge(LeftSorted, RightSorted, Sorted), !.
