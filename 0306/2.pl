connection(gliwice, wroclaw). 
connection(wroclaw, poznan). 
connection(poznan, gdansk). 
connection(gdansk, warszawa). 
connection(warszawa, gdansk). 
connection(poznan, gliwice). 
connection(warszawa, gliwice).

trip(S, E, T) :-
	trip(S, E, T, []).

trip(S, S, [S|R], R).
trip(S, E, R, T) :-
	connection(W, E),
	\+ member(W, T),
	trip(S, W, R, [E|T]).
