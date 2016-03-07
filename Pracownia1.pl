% Maciej Szeptuch 2012
% Zadanie Wersja 8 - Magnesy

% Format pliku wejściowego:
%%	Liczba wierszy
%%	Liczba kolumn
%%	Mapa
%%	Liczba północnych biegunów w kolumnach
%%	Liczba północnych biegunów w wierszach
%%	Liczba południowych biegunów w kolumnach
%%	Liczba południowych biegunów w wierszach

% Rozwiązanie. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Brute force. Próbuje w każdym miejscu postawić biegun N lub S lub ich nie 
%%	stawiać, po przejściu całego wiersza muszą się zgadzać liczby z biegunów 
%%	z wejścia, i po przejściu całej "mapy" muszą się zgadzać liczby w kolumnach
%%	(w moim przypadku za każdym razem jak ustawie biegun to zmniejszam odpowiednie
%%	wartości, więc wystarczy sprawdzić czy zostały listy samych zer). 
%%	Przy ustawianiu bieguna biorę pod uwagę pole nad i na lewo od niego.
%%
% Testy. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%		Proste testy poprawnościowe (czas:  ~1s)
%%		Np.
%%		10.
%%		10.
%%		[
%%			[r,l,d,r,l,r,l,d,r,l],
%%			[r,l,u,d,r,l,d,u,r,l],
%%			[r,l,d,u,d,d,u,d,r,l],
%%			[r,l,u,d,u,u,d,u,r,l],
%%			[d,d,d,u,r,l,u,d,d,d],
%%			[u,u,u,d,r,l,d,u,u,u],
%%			[d,r,l,u,r,l,u,r,l,d],
%%			[u,r,l,r,l,r,l,r,l,u],
%%			[d,d,d,d,d,d,d,r,l,d],
%%			[u,u,u,u,u,u,u,r,l,u]
%%		].
%%		[3,3,0,4,2,1,5,1,2,3].
%%		[2,2,3,1,3,2,4,3,2,2].
%%		[3,2,1,5,1,2,4,0,3,3].
%%		[2,2,3,1,3,2,4,3,2,2].
%%
%%		Makstest (czas: ~10h)
%%		16.
%%		16.
%%		[
%%			[r,l,r,l,d,d,r,l,r,l,r,l,d,d,r,l],
%%			[d,r,l,d,u,u,r,l,d,r,l,d,u,u,r,l],
%%			[u,d,d,u,r,l,r,l,u,d,d,u,r,l,r,l],
%%			[d,u,u,r,l,r,l,d,d,u,u,r,l,r,l,d],
%%			[u,r,l,d,d,d,d,u,u,r,l,d,d,d,d,u],
%%			[r,l,d,u,u,u,u,d,r,l,d,u,u,u,u,d],
%%			[d,d,u,r,l,r,l,u,d,d,u,r,l,r,l,u],
%%			[u,u,r,l,r,l,r,l,u,u,r,l,r,l,r,l],
%%			[r,l,r,l,d,d,r,l,r,l,r,l,d,d,r,l],
%%			[d,r,l,d,u,u,r,l,d,r,l,d,u,u,r,l],
%%			[u,d,d,u,r,l,r,l,u,d,d,u,r,l,r,l],
%%			[d,u,u,r,l,r,l,d,d,u,u,r,l,r,l,d],
%%			[u,r,l,d,d,d,d,u,u,r,l,d,d,d,d,u],
%%			[r,l,d,u,u,u,u,d,r,l,d,u,u,u,u,d],
%%			[d,d,u,r,l,r,l,u,d,d,u,r,l,r,l,u],
%%			[u,u,r,l,r,l,r,l,u,u,r,l,r,l,r,l]
%%		].
%%		[6,4,6,2,6,4,6,8,4,6,6,2,6,4,6,8].
%%		[7,3,4,6,4,5,7,6,7,3,4,6,4,5,7,6].
%%		[4,6,6,4,4,2,8,8,6,4,6,4,4,2,8,8].
%%		[7,3,4,6,6,5,5,6,7,3,4,6,6,5,5,6].

% Główna funkcja. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Wczytuje dane z pliku, generuje wynik oraz wypisuje go "ładnie" na wyjście.
solvePuzzle(Input, Result) :-
	readData(Input, Cols, [Row|Map], ColsNorth, ColsSouth, RowsNorth, RowsSouth),
	tryThemAll([Row|Map], ColsNorth, ColsSouth, RowsNorth, RowsSouth, Row, Result),
	writeData(Result, Cols, ColsNorth, ColsSouth, RowsNorth, RowsSouth).

% Funkcja sprawdzająca wszystkie możliwości.
%%	Próbuje ustawić najwyższy wiersz i odpala się dla pozostałej mapy.
tryThemAll([Row|Map], ColsNorth, ColsSouth, [North|RowsNorth], [South|RowsSouth], UpperResRow, [ResRow|Result]) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, UpperResRow, ResRow),
	tryThemAll(Map, NewColsNorth, NewColsSouth, RowsNorth, RowsSouth, ResRow, Result).

% Warunek końcowy(z opisu, same zera w tablicy biegunów).
tryThemAll([], ColsNorth, ColsSouth, [], [], _, []) :-
	onlyZeroes(ColsNorth),
	onlyZeroes(ColsSouth).

% Ustawianie wiersza. Potrzebuje znać co jest nad i przed danym polem.
tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, UpperResRow, ResRow) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, q, UpperResRow, ResRow).

% Warunek końcowy dla wiersza(nie ma już więcej pól i liczby biegunów = 0)
tryRow([], [], [], [], [], 0, 0, _, [], []).

%% Ustalanie nowej wartości dla u/uX
% Ustawianie bieguna północnego na u/uN <=> nad jest dS, poprzedni nie był północnym i można jeszcze dostawiać.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth2|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, Before, [dS|UpperResRow], [uN|ResRow]) :-
	(H = u; H = uN),
	North > 0, CNorth > 0,
	\+ north(Before),
	North2 is North - 1,
	CNorth2 is CNorth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North2, South, uN, UpperResRow, ResRow).

% Ustawianie bieguna południowego na u/uS <=> nad jest dN, poprzedni nie był połódniowym i można jeszcze dostawiać.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth2|NewColsSouth], North, South, Before, [dN|UpperResRow], [uS|ResRow]) :-
	(H = u; H = uS),
	South > 0, CSouth > 0,
	\+ south(Before),
	South2 is South - 1,
	CSouth2 is CSouth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South2, uS, UpperResRow, ResRow).

% Zostawienie w aktualnym polu u <=> ponad jest d.
tryRow([u|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, _, [d|UpperResRow], [u|ResRow]) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, u, UpperResRow, ResRow).

%% Ustalanie nowej wartości dla l/lX
% Ustawianie bieguna północnego na l/lN <=> na lewo rS, nad nie jest północny i można jeszcze dostawiać.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth2|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, rS, [Upper|UpperResRow], [lN|ResRow]) :-
	(H = l; H = lN),
	North > 0, CNorth > 0,
	\+ north(Upper),
	North2 is North - 1,
	CNorth2 is CNorth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North2, South, lN, UpperResRow, ResRow).

% Ustawianie bieguna południowego na l/lS <=> na lewo rN, nad nie jest południowy i można jeszcze dostawiać.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth2|NewColsSouth], North, South, rN, [Upper|UpperResRow], [lS|ResRow]) :-
	(H = l; H = lS),
	South > 0, CSouth > 0,
	\+ south(Upper),
	South2 is South - 1,
	CSouth2 is CSouth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South2, lS, UpperResRow, ResRow).

% Zostawienie w aktualnym polu l <=> na lewo jest r.
tryRow([l|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, r, [_|UpperResRow], [l|ResRow]) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, l, UpperResRow, ResRow).

%% Ustalanie nowej wartości dla d/dX
% Ustawianie bieguna północnego na d/dN <=> na lewo i nad nie ma północnych i można jeszcze dostawiać, pole poniżej sprawdzi czy będzie ok.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth2|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, Before, [Upper|UpperResRow], [dN|ResRow]) :-
	(H = d; H = dN),
	North > 0, CNorth > 0, CSouth > 0,
	\+ north(Before), \+ north(Upper),
	North2 is North - 1,
	CNorth2 is CNorth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North2, South, dN, UpperResRow, ResRow).

% Ustawianie bieguna południowego na d/dS <=> na lewo i nad nie ma południowych i można jeszcze dostawiać, pole poniżej sprawdzi czy będzie ok.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth2|NewColsSouth], North, South, Before, [Upper|UpperResRow], [dS|ResRow]) :-
	(H = d; H = dS),
	South > 0, CSouth > 0, CNorth > 0,
	\+ south(Before), \+ south(Upper),
	South2 is South - 1,
	CSouth2 is CSouth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South2, dS, UpperResRow, ResRow).

% Zostawienie w danym polu d. Nie ma warunów, pole poniżej sprawdzi czy będzie ok. 
tryRow([d|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, _, [_|UpperResRow], [d|ResRow]) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, d, UpperResRow, ResRow).

%% Ustalanie nowej wartości dla r/rX
% Ustawianie bieguna północnego na r/rN <=> na lewo i nad nie ma północnych i można jeszcze dostawiać, następny sprawdzi czy będzie ok.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth2|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, Before, [Upper|UpperResRow], [rN|ResRow]) :-
	(H = r; H = rN),
	North > 0, CNorth > 0, South > 0,
	\+ north(Before), \+ north(Upper),
	North2 is North - 1,
	CNorth2 is CNorth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North2, South, rN, UpperResRow, ResRow).

% Ustawianie bieguna południowego na r/rS <=> na lewo i nad nie ma południowych i można jeszcze dostawiać, następny sprawdzi czy będzie ok.
tryRow([H|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth2|NewColsSouth], North, South, Before, [Upper|UpperResRow], [rS|ResRow]) :-
	(H = r; H = rS),
	South > 0, CSouth > 0, North > 0,
	\+ south(Before), \+ south(Upper),
	South2 is South - 1,
	CSouth2 is CSouth - 1,
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South2, rS, UpperResRow, ResRow).

% Zostawienie w danym polu r. Nie ma warunków, następne pole sprawdzi czy będzie ok.
tryRow([r|Row], [CNorth|ColsNorth], [CNorth|NewColsNorth], [CSouth|ColsSouth], [CSouth|NewColsSouth], North, South, _, [_|UpperResRow], [r|ResRow]) :-
	tryRow(Row, ColsNorth, NewColsNorth, ColsSouth, NewColsSouth, North, South, r, UpperResRow, ResRow).

onlyZeroes([0|T]) :- onlyZeroes(T).
onlyZeroes([]).

south(X) :- X = lS; X = dS; X = uS; X = rS.
north(X) :- X = lN; X = dN; X = uN; X = rN.

% Wczytywanie danych. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
readData(Filename, Cols, Map, ColsNorth, ColsSouth, RowsNorth, RowsSouth) :-
	see(Filename),
	read(_),
	read(Cols),
	read(Map),
	read(ColsNorth),
	read(RowsNorth),
	read(ColsSouth),
	read(RowsSouth),
	seen.

% "Ładne" wypisywanie. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wypisz etykiety kolumn
writeHoriz([]) :- nl, !.
writeHoriz([N|T]) :- !, write(' '), write(N), write('  '), writeHoriz(T).

%% Wypisz elementy listy
writeList([]) :- !.
writeList([H|T]) :- !, write(H), writeList(T).

%% Wypisz X n razy
writeTimes(_, 0) :- !.
writeTimes(Text, N) :- !, write(Text), N1 is N - 1, writeTimes(Text, N1).

%% Wypisz wiersze z mapy
writeRows([], _, _) :- !.
writeRows([Row|Map], [North|RowsNorth], [South|RowsSouth]) :- !,
	write(' '), write(North), write(' |'), writeRow(Row, Down), write(' '), write(South), nl,
	write('   |'), writeList(Down), nl,
	writeRows(Map, RowsNorth, RowsSouth).

writeRow([], []) :- !.
writeRow([H], ['----']) :- (H = u; H = l), !, write('   |').
writeRow([d], ['   |']) :- !, write('   |').
writeRow([H], ['----']) :- (H = uN; H = lN), !, write(' N |').
writeRow([dN], ['   |']) :- !, write(' N |').
writeRow([H], ['----']) :- (H = uS; H = lS), !, write(' S |').
writeRow([dS], ['   |']) :- !, write(' S |').

writeRow([H|T], ['----'|TX]) :- (H = u; H = l), !, write('   |'), writeRow(T, TX).
writeRow([r|T], ['----'|TX]) :- !, write('    '), writeRow(T, TX).
writeRow([d|T], ['   |'|TX]) :- !, write('   |'), writeRow(T, TX).

writeRow([H|T], ['----'|TX]) :- (H = uN; H = lN), !, write(' N |'), writeRow(T, TX).
writeRow([rN|T], ['----'|TX]) :- !, write(' N  '), writeRow(T, TX).
writeRow([dN|T], ['   |'|TX]) :- !, write(' N |'), writeRow(T, TX).

writeRow([H|T], ['----'|TX]) :- (H = uS; H = lS), !, write(' S |'), writeRow(T, TX).
writeRow([rS|T], ['----'|TX]) :- !, write(' S  '), writeRow(T, TX).
writeRow([dS|T], ['   |'|TX]) :- !, write(' S |'), writeRow(T, TX).

%% Wypisz wynik
writeData([], _, _, _, _, _) :- !.
writeData(Map, Cols, ColsNorth, ColsSouth, RowsNorth, RowsSouth) :-
	write('    '), writeHoriz(ColsNorth),
	Cols2 is Cols - 1, write('   |'), writeTimes('----', Cols2), write('---|'), nl,
	writeRows(Map, RowsNorth, RowsSouth),
	write('    '), writeHoriz(ColsSouth).
