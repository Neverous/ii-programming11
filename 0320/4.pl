number(s, 0). number(s, 2). number(s, 4). number(s, 6). number(s, 8).
number(c, 1). number(c, 3). number(c, 5). number(c, 7). number(c, 9).

createNumber(Types, Number) :-
	createNumber(Types, Number, 0).

createNumber([], Number, Number).
createNumber([Type|Tail], Number, Acc) :-
	number(Type, N),
	Acc2 is Acc * 10 + N,
	createNumber(Tail, Number, Acc2).

checkSums(_, _, [_], Result, [Result]) :- !.
checkSums(Number1, Number2, [Types|Next], Result, [Head|Tail]) :-
	Number3 is Number2 // 10,
	createNumber(Types, Head),
	Head is Number1 * (Number2 mod 10),
	checkSums(Number1, Number3, Next, Result, Tail), !.

query([TA, TB|Rest], [Number1, Number2|Acc]) :-
	last(Rest, TR),
	createNumber(TA, Number1),
	createNumber(TB, Number2),
	createNumber(TR, Result),
	Result is Number1 * Number2,
	checkSums(Number1, Number2, Rest, Result, Acc).
