insert(Element, node(leaf, Value, Right), node(node(leaf, Element, leaf), Value, Right)) :-
	Element < Value, !.

insert(Element, node(Left, Value, leaf), node(Left, Value, node(leaf, Element, leaf))) :- !.
insert(Element, node(Left, Value, Right), node(NewLeft, Value, Right)) :-
	Element < Value, !,
	insert(Element, Left, NewLeft).

insert(Element, node(Left, Value, Right), node(Left, Value, NewRight)) :-
	insert(Element, Right, NewRight).

insert(Element, leaf, node(leaf, Element, leaf)) :- !.

find(Element, node(_, Element, _)) :- !.
find(Element, node(Left, Value, _)) :-
	Element < Value, !,
	find(Element, Left).

find(Element, node(_, _, Right)) :-
	find(Element, Right).

findMax(Element, node(_, Element, leaf)) :- !.
findMax(Element, node(_, _, Right)) :-
	findMax(Element, Right).

delMax(Element, node(Left, Element, leaf), Left) :- !.
delMax(Element, node(Left, Value, Right), node(Left, Value, NewRight)) :-
	delMax(Element, Right, NewRight).

delete(Element, node(Left, Element, leaf), Left) :- !.
delete(Element, node(Left, Value, Right), node(NewLeft, Value, Right)) :-
	Element < Value, !,
	delete(Element, Left, NewLeft).

delete(Element, node(Left, Value, Right), node(Left, Value, NewRight)) :-
	Element > Value, !,
	delete(Element, Right, NewRight).

delete(Element, node(Left, Element, node(Left2, Value, Right)), node(Left, Value, NewRight)) :-
	!,
	delete(Element, node(Left2, Element, Right), NewRight).

empty(leaf).
