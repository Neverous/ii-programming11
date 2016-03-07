sibling(A, B) :- parent(X, A), parent(X, B), male(X), parent(Y, A), parent(Y, B), female(Y).
sister(A, B) :- sibling(A, B), female(A).
grandson(A, B) :- parent(X, A), parent(B, X), male(A).
cousin(A, B) :- parent(X, B), sibling(X, Y), parent(Y, A), male(A).
descendant(A, B) :- parent(B, A).
descendant(A, B) :- parent(B, X), descendant(A, X).
is_mother(A) :- female(A), parent(A, _).
is_father(A) :- male(A), parent(A, _).

% Drzewko...
male(adam).
male(john).
male(joshua).
male(mark).
male(david).
female(eve).
female(helen).
female(ivonne).
female(anna).
parent(adam, helen).
parent(adam, ivonne).
parent(adam, anna).
parent(eve, helen).
parent(eve, ivonne).
parent(eve, anna).
parent(john, joshua).
parent(helen, joshua).
parent(ivonne, david).
parent(mark, david).
