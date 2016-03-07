:- use_module(library(arithmetic)).
:- arithmetic_function('!'/1).
:- arithmetic_function('!!'/1).
:-op(505, xf, '!').
:-op(507, xf, '!!').
!(N, 1) :- N =< 1, !.
!(N, R) :- N > 1, !, N2 is N - 1, !(N2, R2), R is R2 * N.

'!!'(N, R) :- 0 is N mod 2, !, N2 is N - 1, fact(N2, R).
'!!'(N, R) :- fact(N, R).

fact(N, 1) :- N =< 2, !.
fact(N, R) :- N > 2, !, N2 is N - 2, fact(N2, R2), R is R2 * N.
