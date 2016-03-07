flatten2(List, Result) :-
	flatten2(List, Result, []).

flatten2([], Result, Result).
flatten2([Head|Tail], Result, Acc) :-
	is_list(Head), !,
	flatten2(Tail, Acc1, Acc),
	flatten2(Head, Result, Acc1).

flatten2([Item|Tail], [Item|Result], Acc) :-
	flatten2(Tail, Result, Acc).
