% Maciej Szeptuch 2012
% Pracownia 2 - Parser
% Wzorowany na kodzie TWI

pParseProgram(Code, AbstractTree) :-
	lTokenize(Code, TokenList),
	parseProgram(TokenList, Tree),
	expandTree(Tree, Tree, AbstractTree),
	length(AbstractTree, Length), !,
	info('Parser', "Parsed %d definitions", [Length]),
	debug('Parser', "Abstract tree: %w", [AbstractTree]).

parseProgram(TokenList, AbstractTree) :-
	phrase(program(AbstractTree, 0), TokenList), !.

parseProgram(_, _) :-
	error('Parser', "Invalid input!", []).

pParseExpression(Code, AbstractTree) :-
	lTokenize(Code, TokenList),
	parseExpression(TokenList, AbstractTree), !,
	debug('Parser', "Abstract tree: %w", [AbstractTree]).

parseExpression(TokenList, AbstractTree) :-
	phrase(expression(AbstractTree), TokenList), !.

parseExpression(_, _) :-
	error('Parser', "Invalid expression!", []).

% Domykanie funkcji
expandTree([def(Level, Name, Args, Local)|Tree], Expand, [def(Level, Name, Args, Result)|ExpandedTree]) :-
	append(Local, Expand, Result),
	expandTree(Tree, Expand, ExpandedTree).

expandTree([], _, []) :- !.

% Program
program([Instruction|AbstractTree], Level) -->
	clause2(Instruction, Level), !,
	{debug('Parser', "Parsed definition %w.", [Instruction])},
	program(AbstractTree, Level).

program([], _) --> "", !.

% Blok
block([], _) --> [tBlockEnd], !.
block([Instruction|AbstractTree], Level) -->
	clause2(Instruction, Level), !,
	block(AbstractTree, Level).

% Klauzule
clauses(Instructions, Level) -->
	[tBlockBegin], !, block(Instructions, Level).

clauses([Instruction], Level) -->
	clause2(Instruction, Level).

% Klauzula
clause2(def(Level, Who, What, Where), Level) -->
	variable(Who),
	lambda(What, Expression),
	[tEqual], !,
	expression(Expression),
	localDeclarations(Where, -1).

% Lokalne deklaracje
localDeclarations([], _) --> [tSemicolon], !.
localDeclarations(Instructions, Level) -->
	[tWhere], !,
	clauses(Instructions, Level).

% Reprezentacja funkcji
lambda(lam(Factor, Lambda), Expression) -->
	factor(Factor), !,
	lambda(Lambda, Expression).

lambda(Expression, Expression) --> "", !.

% Skladniki
factor(var(0, Variable)) -->
	variable(Variable), !.

factor(con(Construct)) -->
	construct(Construct), !.

factor(Pattern) -->
	[tBracketOpen], !,
	pattern(Pattern),
	[tBracketClose].

factors(ext(Factor, Factors)) -->
	factor(Factor), !,
	factors(Factors).

factors(end) --> "", !.

% Wzorzec
pattern(var(0, Variable)) -->
	variable(Variable).

pattern(ext(con(Construct), Exps)) -->
	construct(Construct),
	factors(Exps).

% Wyrażenie
expression(exp(lam(var(0, Variable), Expression))) -->
	[tBackslash], !,
	variable(Variable),
	[tArrow], !,
	expression(Expression).

expression(Expression) -->
	(booleanExpression(Expression); arithmeticExpression(Expression)).

% Wyrażenie logiczne
booleanExpression(exp(Expression)) -->
	arithmeticExpression(Exp1),
	[Op], {
		(Op == tLesserThan,			!, Expression = lt(Exp1, Exp2));
		(Op == tGreaterThan,		!, Expression = gt(Exp1, Exp2));
		(Op == tLesserEqualThan,	!, Expression = lte(Exp1, Exp2));
		(Op == tGreaterEqualThan,	!, Expression = gte(Exp1, Exp2));
		(Op == tEqual,				!, Expression = eq(Exp1, Exp2));
		(Op == tNotEqual,			!, Expression = neq(Exp1, Exp2));
		fail
	}, !,
	arithmeticExpression(Exp2).

% Wyrażenie arytmetyczne
arithmeticExpression(Expression) -->
	multiplicativeExpression(Acc), arithmeticExpression(Expression, Acc).

arithmeticExpression(Expression, Acc) -->
	[Op], {
		(Op == tPlus,	!, Component = add(Acc, Exp));
		(Op == tMinus,	!, Component = sub(Acc, Exp));
		fail
	}, !,
	multiplicativeExpression(Exp),
	{Acc1 =.. [exp(Component)]},
	arithmeticExpression(Expression, Acc1).

arithmeticExpression(exp(Exp2), Exp) --> "", !,
	{cleanExp(Exp, Exp2)}.

multiplicativeExpression(Expression) -->
	aplication(Acc), multiplicativeExpression(Expression, Acc).

multiplicativeExpression(Expression, Acc) -->
	[Op], {
		(Op == tMultiply,	!, Component = mul(Acc, Exp));
		(Op == tDivide,		!, Component = div_(Acc, Exp));
		(Op == tModulo,		!, Component = mod_(Acc, Exp));
		fail
	}, !,
	aplication(Exp),
	{Acc1 =.. [exp(Component)]},
	multiplicativeExpression(Expression, Acc1).

multiplicativeExpression(exp(Exp2), Exp) --> "", !,
	{cleanExp(Exp, Exp2)}.

% Aplikacja
aplication(exp(ext(Name, Arg))) -->
	atomic2(Name),
	aplication(Arg, end).

aplication(exp(Atomic)) -->
	atomic2(Ato), !,
	{cleanExp(Ato, Atomic)}.

aplication(exp(ext(Name, Arg)), end) -->
	atomic2(Name),
	aplication(Arg, end).

aplication(exp(ext(Name, end)), end) -->
	atomic2(Name), !.

% Wyrażenie atomowe
atomic2(Expression) -->
	[tBracketOpen], !,
	expression(Expression),
	[tBracketClose].

atomic2(exp(var(0, Variable))) -->
	variable(Variable), !.

atomic2(exp(con(Construct))) -->
	construct(Construct), !.

% Zmienna
variable(Variable) -->
	[tIdentifier([First|Tail])],
	{code_type(First, lower)}, !,
	{atom_codes(Variable, [First|Tail])}.

% Konstruktor
construct(Construct) -->
	[tIdentifier([First|Tail])],
	{code_type(First, upper)}, !,
	{atom_codes(Construct, [First|Tail])}.

construct(Number) -->
	[tNumber(Number)], !.

% Usuwanie nadmiarowych exp powstałych z nawiasowania
cleanExp(exp(X), Y) :- !, cleanExp(X, Y).
cleanExp(X, X) :- !.
