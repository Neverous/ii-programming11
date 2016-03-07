% instruction(am(Code, State, Cont), am(Code2, State2, Cont2))
instruction(am(skip, State, Cont), am(eps, State, Cont)).
instruction(am(assign(X, Value), State, Cont), am(eps, State2, Cont)) :-
	update_state(X, Value, State, State2).
...

