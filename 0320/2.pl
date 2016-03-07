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
	stack_put(Element, Acc, Res),
	gatherResults(Result, Res), !.

gatherResults(Result, Result).

que_empty(X) :-
	var(X),
	X == Y - Y.

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
	que_put(Element, Acc, Res),
	gatherResultsDiff(Result, Res), !.

gatherResultsDiff(Result, Result).

e(1, 2).
e(2, 3).
e(3, 4).
e(3, 5).
e(3, 6).
e(5, 1).
e(5, 7).
e(7, 8).
e(8, 7).

dfs(V1, V2) :-
	stack_empty(Empty),
	stack_put(V1, Empty, Stack),
	dfs2(V2, Stack, []).

dfs2(End, Stack, _) :-
	stack_get(Stack, End, _), !.

dfs2(End, Stack, Visited) :-
	stack_get(Stack, Vert, Stack2),
	member(Vert, Visited), !,
	dfs2(End, Stack2, Visited).

dfs2(End, Stack, Visited) :-
	stack_get(Stack, Vert, Stack2),
	write(End), write('?'), write(Vert), write('\n'),
	stack_addall(N, e(Vert, N), Stack2, Stack3),
	dfs2(End, Stack3, [Vert|Visited]).

bfs(V1, V2) :-
	que_empty(Empty),
	que_put(V1, Empty, Que),
	bfs2(V2, Que, []).

bfs2(End, Que, _) :-
	que_get(Que, End, _), !.

bfs2(End, Que, Visited) :-
	que_get(Que, Vert, Que2),
	member(Vert, Visited), !,
	bfs2(End, Que2, [Vert|Visited]).

bfs2(End, Que, Visited) :-
	que_get(Que, Vert, Que2),
	write(End), write('?'), write(Vert), write('\n'),
	que_addall(N, e(Vert, N), Que2, Que3),
	bfs2(End, Que3, [Vert|Visited]).

