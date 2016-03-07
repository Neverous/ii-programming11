% Maciej Szeptuch 2012
% Pracownia 2 - Maszyna Abstrakcyjna

amLoad(Code, _, NewState) :-
	pParseProgram(Code, NewState),
	checkDefinitions(NewState), !,
	debug('Abstract Machine', "State: %w", [NewState]).

amLoad(_, State, State) :-
	error('Abstract Machine', "Unknown error in file!", []).

amRun(Code, State) :-
	pParseExpression(Code, Expression),
	eval(Expression, State, 1, exp(Result)),
	info('Abstract Machine', "Got: %w!", [Result]),
	(
		(Result = con(Value), Msg = "= %w", Args = [Value]);
		(Result = ext(exp(con(Name)), _), Msg = "= <construct %w>", Args = [Name]);
		(Result = lam(_, _), Msg = "= <function>", Args = [])
	), !,
	writef(Msg, Args), nl.

amRun(_, _) :-
	error('Abstract Machine', "Invalid expression!", []).

% Sprawdzanie definicji i struktury 'pliku'
checkDefinitions([def(_, Name, Args, _)|Definitions]) :-
	checkDefinition(def(Name, Args), Name, Definitions), !,
	checkDefinitions(Definitions).

checkDefinitions([]) :- !.
checkDefinitions([def(_, Name, _, _)|_]) :-
	error('Abstract Machine', "Error in '%w' definitions!", [Name]).

checkDefinition(def(DefName, DefArgs), DefName, [def(_, DefName, ActArgs, _)|Definitions]) :- !,
	checkArguments(DefName, DefArgs, ActArgs),
	checkDefinition(def(DefName, DefArgs), DefName, Definitions).

checkDefinition(def(DefName, _), _, [def(_, DefName, _, _)|_]) :-
	error('Abstract Machine', "Clauses of '%w' are not together!", [DefName]).

checkDefinition(def(DefName, DefArgs), _, [def(_, ActName, ActArgs, _)|Definitions]) :- !,
	checkDefinition(def(DefName, DefArgs), def(ActName, ActArgs), Definitions).

checkDefinition(_, _, []) :- !.

% Liczba argumentów
checkArguments(_, exp(_), exp(_)) :- !.
checkArguments(_, exp(_), ext(_, _)) :- !.
checkArguments(Name, lam(_, DefArgs), lam(_, ActArgs)) :-
	checkArguments(Name, DefArgs, ActArgs).

checkArguments(Name, _, _) :-
	error('Abstract Machine', "Different number of arguments in '%w' definitions!", [Name]).

% Wyrażenie
eval(exp(Exp), _, Level, exp(Exp)) :- debug('Abstract Machine', "{%w} Postponing computation '%w'!", [Level, Exp]).

eval(exp(add(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	Result is Result1 + Result2.

eval(exp(sub(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	Result is Result1 - Result2.

eval(exp(mul(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	Result is Result1 * Result2.

eval(exp(div_(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	Result is Result1 / Result2.

eval(exp(mod_(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	Result is Result1 mod Result2.

eval(exp(eq(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 is Result2, Result = 'True');
		Result = 'False'
	).

eval(exp(neq(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 is Result2, Result = 'False');
		Result = 'True'
	).

eval(exp(lt(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 < Result2, Result = 'True');
		Result = 'False'
	).

eval(exp(lte(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 =< Result2, Result = 'True');
		Result = 'False'
	).

eval(exp(gt(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 > Result2, Result = 'True');
		Result = 'False'
	).

eval(exp(gte(Exp1, Exp2)), State, Level, exp(con(Result))) :- !,
	eval(Exp2, State, Level, exp(con(Result2))), eval(Exp1, State, Level, exp(con(Result1))),
	(
		(Result1 >= Result2, Result = 'True');
		Result = 'False'
	).

eval(exp(con(Value)), _, _, exp(con(Value))) :- !.
eval(exp(var(VLevel, Name)), State, Level, Result) :- nonvar(Name), !,
	debug('Abstract Machine', "{%w} Searching for '%w:%w' variable!", [Level, Name, VLevel]),
	getDefinition(VLevel, Name, State, Function),
	applyFunction(Function, end, State, Level, TResult),
	eval(TResult, State, Level, Result).

eval(exp(ext(Expression, end)), State, Level, Result) :- !,
	eval(Expression, State, Level, Result).

eval(exp(ext(exp(var(VLevel, Name)), Args)), State, Level, Result) :- nonvar(Name), !,
	debug('Abstract Machine', "{%w} Searching for '%w:%w' function!", [Level, Name, VLevel]),
	getDefinition(VLevel, Name, State, Function),
	applyFunction(Function, Args, State, Level, TResult),
	eval(TResult, State, Level, Result).

%% Funkcje anonimowe
eval(exp(ext(exp(lam(Exp, Exps)), Args)), State, Level, Result) :- !,
	applyFunction(def(Level, lambda, lam(Exp, Exps), State), Args, State, Level, TResult),
	eval(TResult, State, Level, Result).

% Szukanie definicji funkcji
getDefinition(Level, Name, [def(Level, Name, Args, Local)|_], def(Level, Name, Args, Local)) :-
	debug('Abstract Machine', "Found '%w:%w'(%w)!", [Name, Level, Args]).

getDefinition(Level, Name, [def(DLevel, DName, DArgs, _)|Definitions], Function) :- !,
	((Name == DName, debug('Abstract Machine', "Looking for '%w:%w' {!= %w:%w(%w)}.", [Name, Level, DName, DLevel, DArgs])); true),
	getDefinition(Level, Name, Definitions, Function).

% Wyliczanie wartości funkcji
applyFunction(def(FLevel, FName, FArgs, FLocal), Args, State, ILevel, Result) :-
	debug('Abstract Machine', "{%w} Computing function %w:%w with %w [want %w].", [ILevel, FName, FLevel, Args, Result]),
	Level is ILevel + 1,
	matchArguments(FArgs, Args, State, Level, RExp, Expand), !,
	debug('Abstract Machine', "{%w} Result expression: %w.", [ILevel, RExp]),
	expandTree(FLocal, FLocal, Local),
	append(Expand, Local, TState),
	findLocals(TState, Locals),
	renameLocals(RExp, Expression, TState, FState, Level, Locals),
	debug('Abstract Machine', "{%w} Renamed: %w.", [ILevel, Expression]),
	eval(Expression, FState, Level, FResult),
	debug('Abstract Machine', "{%w} Before clean: %w.", [ILevel, FResult]),
	resolveArguments(FResult, FState, ILevel, Result),
	debug('Abstract Machine', "{%w} Function (%w:%w) result: %w.", [ILevel, FName, FLevel, Result]).

% "Przenazywanie" zmiennych lokalnych
findLocals([def(VLevel, Name, _, _)|TState], [Name|Locals]) :-
	VLevel < 0, !,
	findLocals(TState, Locals).

findLocals([_|TState], Locals) :- !,
	findLocals(TState, Locals).

findLocals([], []).

renameLocals(Expression, RExpression, [def(-1, Name, Exp, Local)|TState], [def(Level, Name, RExp, Local)|FState], Level, Renamed) :- !,
	debug('Abstract Machine', "{%w} Renaming local %w", [Level, Name]),
	renameArguments(Exp, Level, RExp, Renamed),
	renameLocals(Expression, RExpression, TState, FState, Level, Renamed).

renameLocals(Expression, RExpression, [def(-2, Name, Exp, Local)|TState], [def(Level, Name, Exp, Local)|FState], Level, Renamed) :- !,
	debug('Abstract Machine', "{%w} Renaming variable %w", [Level, Name]),
	renameLocals(Expression, RExpression, TState, FState, Level, Renamed).

renameLocals(Expression, RExpression, [def(ILevel, Name, Exp, Local)|TState], [def(ILevel, Name, Exp, Local)|FState], Level, Renamed) :- !,
	renameLocals(Expression, RExpression, TState, FState, Level, Renamed).

renameLocals(Expression, RExpression, [], [], Level, Renamed) :- !,
	renameArguments(Expression, Level, RExpression, Renamed).

renameArguments(lam(Argument, Arguments), Level, lam(Argument, RArguments), Renamed) :- !,
	renameArguments(Arguments, Level, RArguments, Renamed).

renameArguments(exp(Expression), Level, RExpression, Renamed) :- !,
	renameVariables(exp(Expression), Level, RExpression, Renamed).

renameVariables(ext(exp(Exp), Exps), Level, ext(exp(RExp), RExps), Renamed) :- !,
	renameVariables(Exps, Level, RExps, Renamed),
	renameVariables(Exp, Level, RExp, Renamed).

renameVariables(ext(Exp, Exps), Level, ext(Exp, RExps), Renamed) :- !,
	renameVariables(Exps, Level, RExps, Renamed).

renameVariables(exp(Exp), Level, exp(RExp), Renamed) :- !,
	renameVariables(Exp, Level, RExp, Renamed).

renameVariables(lam(Argument, Arguments), Level, lam(RArgument, RArguments), Renamed) :- !,
	renameVariables(Arguments, Level, RArguments, Renamed),
	renameVariables(Argument, Level, RArgument, Renamed).

renameVariables(end, _, end, _) :- !.
renameVariables(con(Name), _, con(Name), _) :- !.
renameVariables(var(_, Name), Level, var(Level, Name), Renamed) :-
	member(Name, Renamed), !.

renameVariables(var(Level, Name), _, var(Level, Name), _) :- !.
renameVariables(Exp, Level, RExp, Renamed) :- !,
	(
		(Exp = add(Exp1, Exp2),		RExp = add(RExp1, RExp2));
		(Exp = sub(Exp1, Exp2),		RExp = sub(RExp1, RExp2));
		(Exp = mul(Exp1, Exp2),		RExp = mul(RExp1, RExp2));
		(Exp = div_(Exp1, Exp2),	RExp = div_(RExp1, RExp2));
		(Exp = mod_(Exp1, Exp2),	RExp = mod_(RExp1, RExp2));
		(Exp = eq(Exp1, Exp2),		RExp = eq(RExp1, RExp2));
		(Exp = neq(Exp1, Exp2),		RExp = neq(RExp1, RExp2));
		(Exp = gt(Exp1, Exp2),		RExp = gt(RExp1, RExp2));
		(Exp = gte(Exp1, Exp2),		RExp = gte(RExp1, RExp2));
		(Exp = lt(Exp1, Exp2),		RExp = lt(RExp1, RExp2));
		(Exp = lte(Exp1, Exp2),		RExp = lte(RExp1, RExp2))
	), !,
	renameVariables(Exp2, Level, RExp2, Renamed),
	renameVariables(Exp1, Level, RExp1, Renamed).

% Rozszerzanie wyrazenia (łączenie dwóch w jedno)
extendExpression(exp(ext(Exp, Exps)), Args, exp(ext(Exp, RExps))) :- !,
	extendExpression(Exps, Args, RExps).

extendExpression(end, Args, Args) :- !.
extendExpression(Exp, Args, exp(ext(Exp, Args))) :- !.

% Dopasowywanie argumentów
matchArguments(exp(Expression), end, _, _, exp(Expression), []) :- !.
matchArguments(Expression, end, _, _, exp(Expression), []) :- !.
matchArguments(exp(Expression), Arguments, _, _, Extended, []) :- !,
	extendExpression(exp(Expression), Arguments, Extended).

matchArguments(lam(Argument, Args), exp(ext(Exp, Exps)), State, Level, Result, Expand) :- !,
	matchArguments(Args, Exps, State, Level, Result, NExpand),
	debug('Abstract Machine', "{%w} Matching argument %w with %w [%w left]", [Level, Argument, Exp, Exps]),
	matchArguments(Argument, Exp, State, Level, TExpand),
	append(TExpand, NExpand, Expand).

matchArguments(lam(Argument, Args), exp(Expression), State, Level, Args, Expand) :- !,
	debug('Abstract Machine', "{%w} Matching argument %w with %w", [Level, Argument, exp(Expression)]),
	matchArguments(Argument, exp(Expression), State, Level, Expand).

matchArguments(var(_, Name), exp(Expression), State, Level, [def(-2, Name, exp(Expression), State)]) :- !,
	debug('Abstract Machine', "{%w} Matched argument: %w:%w = %w.", [Level, Name, -1, exp(Expression)]).

matchArguments(con(Value), exp(Expression), State, Level, []) :- !,
	eval(exp(Expression), State, Level, exp(con(Value))),
	debug('Abstract Machine', "{%w} Matched cons: %w.", [Level, Value]).

matchArguments(ext(con(Name), Args), exp(Exp), State, Level, Expand) :- !,
	debug('Abstract Machine', "{%w} Matching construct %w with %w.", [Level, ext(con(Name), Args), Exp]),
	eval(exp(Exp), State, Level, exp(ext(exp(con(Name)), Exps))),
	matchArguments(Args, Exps, State, Level, Expand).

matchArguments(ext(Arg, Args), exp(ext(Exp, Exps)), State, Level, Expand) :- !,
	matchArguments(Args, Exps, State, Level, NExpand),
	debug('Abstract Machine', "{%w} Matching subargument %w with %w [%w left]", [Level, Arg, Exp, Exps]),
	matchArguments(Arg, Exp, State, Level, TExpand),
	append(TExpand, NExpand, Expand).

matchArguments(end, end, _, _, []) :- !.

% Pozbywanie się zmiennych lokalnych
resolveArguments(exp(Expression), State, Level, exp(RExpression)) :- !,
	resolveArguments(Expression, State, Level, RExpression).

resolveArguments(lam(Argument, Arguments), State, Level, lam(RArgument, RArguments)) :- !,
	resolveArguments(Arguments, State, Level, RArguments),
	debug('Abstract Machine', "{%w} Resolving argument %w[%w]", [Level, Argument, RArgument]),
	resolveArguments(Argument, State, Level, RArgument).

resolveArguments(ext(Exp, Exps), State, Level, ext(RExp, RExps)) :- !,
	resolveArguments(Exps, State, Level, RExps),
	debug('Abstract Machine', "{%w} Resolving expression %w[%w]", [Level, Exp, RExp]),
	resolveArguments(Exp, State, Level, RExp).

resolveArguments(con(Name), _, _, con(Name)) :- !.

resolveArguments(var(VLevel, Name), _, Level, var(VLevel, Name)) :-
	VLevel < Level, !,
	debug('Abstract Machine', "{%w} %w may stay.", [Level, var(VLevel, Name)]).

resolveArguments(var(VLevel, Name), State, Level, Result) :- !,
	eval(exp(var(VLevel, Name)), State, Level, exp(Result)),
	Result \= var(VLevel, Name), !,
	debug('Abstract Machine', "{%w} %w resolved to %w.", [Level, var(VLevel, Name), Result]).

resolveArguments(end, _, _, end) :- !.
resolveArguments(Exp, State, Level, RExp) :- !,
	(
		(Exp = add(Exp1, Exp2),		RExp = add(RExp1, RExp2));
		(Exp = sub(Exp1, Exp2),		RExp = sub(RExp1, RExp2));
		(Exp = mul(Exp1, Exp2),		RExp = mul(RExp1, RExp2));
		(Exp = div_(Exp1, Exp2),	RExp = div_(RExp1, RExp2));
		(Exp = mod_(Exp1, Exp2),	RExp = mod_(RExp1, RExp2));
		(Exp = eq(Exp1, Exp2),		RExp = eq(RExp1, RExp2));
		(Exp = neq(Exp1, Exp2),		RExp = neq(RExp1, RExp2));
		(Exp = gt(Exp1, Exp2),		RExp = gt(RExp1, RExp2));
		(Exp = gte(Exp1, Exp2),		RExp = gte(RExp1, RExp2));
		(Exp = lt(Exp1, Exp2),		RExp = lt(RExp1, RExp2));
		(Exp = lte(Exp1, Exp2),		RExp = lte(RExp1, RExp2))
	), !,
	resolveArguments(Exp2, State, Level, RExp2),
	resolveArguments(Exp1, State, Level, RExp1).
