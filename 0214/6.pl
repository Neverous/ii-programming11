% polaczenie(wroclaw, warszawa).
% polaczenie(wroclaw, krakow).
polaczenie(wroclaw, szczecin).
polaczenie(szczecin, lublin).
polaczenie(lublin, szczecin).
% polaczenie(szczecin, gniezno).
% polaczenie(warszawa, katowice).
% polaczenie(gniezno, gliwice).
polaczenie(lublin, gliwice).
polaczenie(gliwice, gniezno).

connection(From, To) :- polaczenie(From, To).
connection(From, To) :- polaczenie(From, X), connection(X, To).
