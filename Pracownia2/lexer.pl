% Maciej Szeptuch 2012
% Pracownia 2 - Lekser
% Wzorowany na kodzie TWI

lTokenize(Code, TokenList) :-
	tokenize(Code, TokenList), !,
	debug('Lexer', "Token list: %w", [TokenList]).

lTokenize(_, _) :-
	error('Lexer', "Invalid input!", []).

tokenize(Code, TokenList) :-
	phrase(lexer(TokenList, 1), Code), !.

% Białe znaki
whiteSpace(Byte, NewByte) -->
	[Char],
	{code_type(Char, space), NewByte is Byte + 1}.

whiteSpaces(ActByte, NewByte) -->
	whiteSpace(ActByte, Byte), !,
	whiteSpaces(Byte, NewByte).

whiteSpaces(Byte, Byte) --> "", !.

% Cyfry
digit(Digit, Byte, NewByte) -->
	[Digit],
	{code_type(Digit, digit), NewByte is Byte + 1}.

digits([Digit|Digits], ActByte, NewByte) -->
	digit(Digit, ActByte, Byte), !,
	digits(Digits, Byte, NewByte).

digits([], Byte, Byte) --> "", !.

% Liczby
number(Number, ActByte, NewByte) -->
	digit(Digit, ActByte, Byte), !,
	digits(Digits, Byte, NewByte),
	{number_chars(Number, [Digit|Digits])}.

% Ciągi liter
letter(Letter, Byte, NewByte) -->
	[Letter],
	{code_type(Letter, alpha), NewByte is Byte + 1}.

letters([Letter|Letters], ActByte, NewByte) -->
	letter(Letter, ActByte, Byte), !,
	letters(Letters, Byte, NewByte).

letters([], Byte, Byte) --> "", !.

% Identyfikator
namechar(Char, Byte, NewByte) -->
	[Char],
	{code_type(Char, csym); code_type(Char, quote)},
	{NewByte is Byte + 1}.

namechars([Char|String], ActByte, NewByte) -->
	namechar(Char, ActByte, Byte), !,
	namechars(String, Byte, NewByte).

namechars([], Byte, Byte) --> "", !.

identifier([Letter|String], ActByte, NewByte) -->
	letter(Letter, ActByte, Byte), !,
	namechars(String, Byte, NewByte).

% Ciąg znaków
char(Char, Byte, NewByte) -->
	[Char],
	{\+ code_type(Char, space), NewByte is Byte + 1}.

chars([Char|Chars], ActByte, NewByte) -->
	char(Char, ActByte, Byte), !,
	chars(Chars, Byte, NewByte).

chars([], Byte, Byte) --> "", !.

string([Char|Chars], ActByte, NewByte) -->
	char(Char, ActByte, Byte), !,
	chars(Chars, Byte, NewByte).

% Komentarze
comment(Comment, ActByte, NewByte) -->
	"--", !,
	{Byte is ActByte + 1},
	untilNewLine(Comment, Byte, NewByte).

untilNewLine([], Byte, NewByte) --> "\n", !,
	{NewByte is Byte + 1}.

untilNewLine([Char|Text], ActByte, NewByte) -->
	[Char], !,
	{Byte is ActByte + 1},
	untilNewLine(Text, Byte, NewByte).

untilNewLine([], Byte, Byte) --> "", !.

% Tokeny
lexer(TokenList, ActByte) -->
	whiteSpaces(ActByte, Byte),
	((comment(_, Byte, NewByte),				!, lexer(TokenList, NewByte));
	((
		(number(Number, Byte, NewByte),			!, {Token = tNumber(Number)});
		("{",									!, {Token = tBlockBegin,		NewByte is Byte + 1});
		("}",									!, {Token = tBlockEnd,			NewByte is Byte + 1});
		(";",									!, {Token = tSemicolon,			NewByte is Byte + 1});
		("\\",									!, {Token = tBackslash,			NewByte is Byte + 1});
		("->",									!, {Token = tArrow,				NewByte is Byte + 2});
		("(",									!, {Token = tBracketOpen,		NewByte is Byte + 1});
		(")",									!, {Token = tBracketClose,		NewByte is Byte + 1});
		("<=",									!, {Token = tLesserEqualThan,	NewByte is Byte + 2});
		(">=",									!, {Token = tGreaterEqualThan,	NewByte is Byte + 2});
		("<",									!, {Token = tLesserThan,		NewByte is Byte + 1});
		(">",									!, {Token = tGreaterThan,		NewByte is Byte + 1});
		("=",									!, {Token = tEqual,				NewByte is Byte + 1});
		("/=",									!, {Token = tNotEqual,			NewByte is Byte + 2});
		("+",									!, {Token = tPlus,				NewByte is Byte + 1});
		("-",									!, {Token = tMinus,				NewByte is Byte + 1});
		("*",									!, {Token = tMultiply,			NewByte is Byte + 1});
		(identifier(Identifier, Byte, NewByte),	!, {
											(member((Identifier, Token), [
												("where", tWhere),
												("div", tDivide),
												("mod", tModulo)
											]), !);
											Token = tIdentifier(Identifier)
											});
		([Code],								!, {error('Lexer', "Unknown character '%s' at byte %d!", [[Code], Byte])})
	), !,
	{TokenList = [Token|TokenList2]},
	lexer(TokenList2, NewByte)
	)).

lexer([], Byte) --> whiteSpaces(Byte, NewByte), {Bytes is NewByte - 1, info('Lexer', "Read %d bytes!", [Bytes])}.
