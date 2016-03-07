split([], _, [], []).
split([Head|Tail], Medium, [Head|Smaller], Bigger) :-
	Head < Medium, !,
	split(Tail, Medium, Smaller, Bigger).

split([Head|Tail], Medium, Smaller, [Head|Bigger]) :-
	split(Tail, Medium, Smaller, Bigger).

qsort(List, Sorted) :-
	qsort(List, Sorted, []).

qsort([], Sorted, Sorted).
qsort([Head|List], Sorted, Acc) :-
	split(List, Head, Smaller, Bigger),
	qsort(Bigger, AlmostSorted, Acc),
	qsort(Smaller, Sorted, [Head|AlmostSorted]).
	

