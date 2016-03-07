% STATE
lookup_state([[Name, Value]|_], Name, Value) :- !.
lookup_state([_|T], Name, Value) :-
	lookup_state(T, Name, Value).

update_state([], Name, Value, [[Name, Value]]) :- !.
update_state([[Name, _]|T], Name, Value, [[Name, Value]|T]) :- !.
update_state([H|T], Name, Value, [H|NewState]) :-
	update_state(T, Name, Value, NewState).

% ARITHMETIC
aexp(add(A1, A2), State, Number) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Number is Result1 + Result2.

aexp(sub(A1, A2), State, Number) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Number is Result1 - Result2.

aexp(mul(A1, A2), State, Number) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Number is Result1 * Result2.

aexp(div(A1, A2), State, Number) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Number is Result1 / Result2.

aexp(num(Number), _, Number).
aexp(var(Variable), State, Number) :-
	lookup_state(State, Variable, Number).

% LOGIC
bexp(true, _, true).
bexp(false, _, false).
bexp(eq(A1, A2), State, true) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Result1 is Result2, !.

bexp(eq(_, _), _, false).
bexp(gte(A1, A2), State, true) :-
	aexp(A1, State, Result1),
	aexp(A2, State, Result2),
	Result1 =< Result2, !.

bexp(gte(_, _), _, false).
bexp(not(B1), State, true) :-
	bexp(B1, State, false).

bexp(not(B1), State, false) :-
	bexp(B1, State, true).

bexp(and(B1, B2), State, true) :-
	bexp(B1, State, true),
	bexp(B2, State, true), !.

bexp(and(_, _), _, false).

% INSTRUCTION
sexp(assign(var(Variable), A), State, NewState) :-
	aexp(A, State, Result),
	update_state(State, Variable, Result, NewState).

sexp(skip, State, State).
sexp(combine(S1, S2), State, NewState) :-
	sexp(S1, State, Through),
	sexp(S2, Through, NewState).

sexp(if(B, S1, _), State, NewState) :-
	bexp(B, State, true), !,
	sexp(S1, State, NewState).

sexp(if(_, _, S2), State, NewState) :-
	sexp(S2, State, NewState).

sexp(while(B, S), State, NewState) :-
	bexp(B, State, true), !,
	sexp(S, State, Through),
	sexp(while(B, S), Through, NewState).

sexp(while(_, _), State, State).
