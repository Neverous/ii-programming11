% Maciej Szeptuch 2012
% Pracownia 2 - Główny plik

:- include('lexer.pl').
:- include('parser.pl').
:- include('am.pl').

error(Name, Message, Args) :- !, swritef(Info, "ERROR[%w]: %s", [Name, Message]), swritef(Error, Info, Args), throw(Error).

%annoy :- !. % to disable stops
annoy :-
	write('Press enter to continue! '), 
	read_line_to_codes(user_input, _).

debug(_, _, _) :- !. % to disable debug
debug(Name, Message, Args) :- writef("DEBUG[%w]: ", [Name]), writef(Message, Args), nl, annoy. % to enable debug

%info(_, _, _) :- !. % to disable info
info(Name, Message, Args) :- writef("INFO[%w]: ", [Name]), writef(Message, Args), nl. % to enable info

% Pętla interpretera
runInterpreter(State) :-
	OldState = State,
	catch(
		(processInput(Command, Arguments), !, runCommand(Command, Arguments, State, NewState)),
		Message,
		(write(Message), nl, NewState = OldState)
	), !,
	nl, runInterpreter(NewState).

% Przetwarzanie wejścia
input(Command, Arguments) -->
	":", !,
	string(Command, 2, AByte), !,
	whiteSpaces(AByte, BByte),
	untilNewLine(Arguments, BByte, CByte),
	shouldBeNothing(CByte).

shouldBeNothing(Byte) --> [Code], {error('Interpreter', "Unknown character '%s' at byte %d!", [[Code], Byte])}.
shouldBeNothing(_) --> "".

processInput(Command, Arguments) :-
	read_line_to_codes(user_input, Line),
	(
		(Line == end_of_file, !, Command = "q");
		(Line == "", !, Command = "nop");
		(phrase(input(Command, Arguments), Line), !);
		error('Interpreter', "Invalid input", [])
	).

% Komendy
runCommand(Command, [], _, _) :- (Command == "q"; Command == "quit"), !,
	write('Bye!'), nl,
	halt.

runCommand(Command, Filename, State, NewState) :- (Command == "l"; Command == "load"), !,
	(
		(readFile(Filename, Code), amLoad(Code, State, NewState), !, writef("%s loaded.", [Filename]));
		NewState = []
	).

runCommand(Command, Code, State, State) :- (Command == "e"; Command == "eval"), !,
	(
		amRun(Code, State);
		true
	).

runCommand("nop", _, State, State) :- !.

runCommand(_, _, _, _) :-
	error('Interpreter', "Unknown command!", []).

% Wczytywanie kodu z pliku
readFile(Filename, Code) :-
	atom_codes(Name, Filename),
	catch(
		open(Name, read, File),
		_,
		error('Interpreter', "Can\'t open file!", [])
	),
	read_stream_to_codes(File, Code),
	close(File).

:- info('Interpreter', "Edit main.pl aby enable/disable debug messages.", []).
:- runInterpreter([]).
:- halt.
