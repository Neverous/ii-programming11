stack_put(Element, Stack, [Element|Stack]).
stack_get([Element|Stack], Element, Stack).
stack_empty([]).
stack_addall(Element, Cond, Stack, Result) :-
	findAll(Element, Cond),
	gatherResults(Result, Stack).
% or:
%	findall(Element, Cond, Temp),
%	append(Temp, Stack, Result).

findAll(Element, Cond) :-
	call(Cond),
	asserta(idontevenknow(Element)),
	fail.

findAll(_, _).
gatherResults(Result, Acc) :-
	idontevenknow(Element),
	retract(idontevenknow(Element)),
	write(Element),
	stack_put(Element, Acc, Res),
	gatherResults(Result, Res), !.

gatherResults(Result, Result).

que_empty(X-X).
diff_append(X-Y, Y-Z, X-Z).
que_put(Element, X-Y, X-Z) :-
	diff_append(X-Y, [Element|Z]-Z, X-Z).
que_get([Element|X]-Y, Element, X-Y).
que_addall(Element, Cond, Que, Result) :-
	findAll(Element, Cond),
	gatherResultsDiff(Result, Que).
% or:
%	findall(Element, Cond, Temp),
%	appendAll(Que, Temp, Result).
%
%appendAll(Acc, [H|T], Result) :-
%	que_put(H, Acc, Acc2),
%	appendAll(Acc2, T, Result), !.

appendAll(Result, [], Result).

gatherResultsDiff(Result, Acc) :-
	idontevenknow(Element),
	retract(idontevenknow(Element)),
	write(Element),
	que_put(Element, Acc, Res),
	gatherResultsDiff(Result, Res), !.

gatherResultsDiff(Result, Result).
