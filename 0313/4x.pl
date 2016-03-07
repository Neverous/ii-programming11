halve(Total-Diff, L, R-Diff) :-
	halve(Total, Total, Total-Diff, L, R).

halve(Total, Tail, X-Y, Total-Tail, Tail) :-
	X == Y.

halve(Total, Tail, [_|X]-Y, Total-Tail, Tail) :-
	X == Y.

halve(Total, [_|T], [_,_|X]-Y, L, R) :-
	halve(Total, T, X-Y, L, R).
