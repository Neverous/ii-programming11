dodaj([], [1]).
dodaj([H|T], [1|T]) :- H = 0.
dodaj([H|T], [0|S]) :- H = 1, dodaj(T, S).

rbin(X) :- rbin(X, [0]).
rbin(X, X).
rbin(X, Y) :- dodaj(Y, Z), rbin(X, Z).
