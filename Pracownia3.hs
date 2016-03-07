-- Maciej Szeptuch 2012
-- Zadanie Wersja 8 - Magnesy

-- Format pliku wejściowego: ---------------------------------------------------
--	Liczba wierszy
--	Liczba kolumn
--	Mapa (W jednej lini!)
--	Liczba północnych biegunów w kolumnach
--	Liczba północnych biegunów w wierszach
--	Liczba południowych biegunów w kolumnach
--	Liczba południowych biegunów w wierszach

-- Rozwiązanie. ----------------------------------------------------------------
--	Brute force. Próbuje w każdym miejscu postawić biegun N lub S magnesu lub ich nie 
--	stawiać, po przejściu całego wiersza muszą się zgadzać liczby z biegunów 
--	z wejścia, i po przejściu całej kolumny muszą się zgadzać liczby w niej.
--
--	Uwagi. ---------------------------------------------------------------------
--	ghc -O2 -optc-O3 Pracownia3.hs && ./Pracownia3

-- Testy(sformatowane dla czytelności). ----------------------------------------
--	Proste testy poprawnościowe (czas:  ~1s)
--	Np.
--	10
--	10
--	[
--		["r","l","d","r","l","r","l","d","r","l"],
--		["r","l","u","d","r","l","d","u","r","l"],
--		["r","l","d","u","d","d","u","d","r","l"],
--		["r","l","u","d","u","u","d","u","r","l"],
--		["d","d","d","u","r","l","u","d","d","d"],
--		["u","u","u","d","r","l","d","u","u","u"],
--		["d","r","l","u","r","l","u","r","l","d"],
--		["u","r","l","r","l","r","l","r","l","u"],
--		["d","d","d","d","d","d","d","r","l","d"],
--		["u","u","u","u","u","u","u","r","l","u"]
--	]
--	[3,3,0,4,2,1,5,1,2,3]
--	[2,2,3,1,3,2,4,3,2,2]
--	[3,2,1,5,1,2,4,0,3,3]
--	[2,2,3,1,3,2,4,3,2,2]
--
--	Makstest (czas: ~10h)
--	16
--	16
--	[
--		["r","l","r","l","d","d","r","l","r","l","r","l","d","d","r","l"],
--		["d","r","l","d","u","u","r","l","d","r","l","d","u","u","r","l"],
--		["u","d","d","u","r","l","r","l","u","d","d","u","r","l","r","l"],
--		["d","u","u","r","l","r","l","d","d","u","u","r","l","r","l","d"],
--		["u","r","l","d","d","d","d","u","u","r","l","d","d","d","d","u"],
--		["r","l","d","u","u","u","u","d","r","l","d","u","u","u","u","d"],
--		["d","d","u","r","l","r","l","u","d","d","u","r","l","r","l","u"],
--		["u","u","r","l","r","l","r","l","u","u","r","l","r","l","r","l"],
--		["r","l","r","l","d","d","r","l","r","l","r","l","d","d","r","l"],
--		["d","r","l","d","u","u","r","l","d","r","l","d","u","u","r","l"],
--		["u","d","d","u","r","l","r","l","u","d","d","u","r","l","r","l"],
--		["d","u","u","r","l","r","l","d","d","u","u","r","l","r","l","d"],
--		["u","r","l","d","d","d","d","u","u","r","l","d","d","d","d","u"],
--		["r","l","d","u","u","u","u","d","r","l","d","u","u","u","u","d"],
--		["d","d","u","r","l","r","l","u","d","d","u","r","l","r","l","u"],
--		["u","u","r","l","r","l","r","l","u","u","r","l","r","l","r","l"]
--	]
--	[6,4,6,2,6,4,6,8,4,6,6,2,6,4,6,8]
--	[7,3,4,6,4,5,7,6,7,3,4,6,4,5,7,6]
--	[4,6,6,4,4,2,8,8,6,4,6,4,4,2,8,8]
--	[7,3,4,6,6,5,5,6,7,3,4,6,6,5,5,6]

import System.IO (putStr, putStrLn, getLine, hGetLine, openFile, IOMode (ReadMode), hClose, Handle)
import Control.Monad (when, filterM)
import Data.Bits ((.|.), (.&.))
import Data.Array.IO (IOUArray, readArray, writeArray, freeze, thaw, newListArray, getElems)
import Data.Array.Base (unsafeWrite, unsafeRead, unsafeFreeze, unsafeThaw, UArray)

data Matrix = Matrix {
	act :: (Int, Int),						-- aktualny wiersz, kolumna
	result :: IOUArray (Int, Int) Int,		-- aktualny "wynik"
	cols :: Int,							-- liczba kolumn
	rows :: Int,							-- liczba wierszy
	northCols :: IOUArray Int Int,			-- północne bieguny w kolumnach
	southCols :: IOUArray Int Int,			-- południowe bieguny w kolumnach
	northRows :: IOUArray Int Int,			-- północne bieguny w wierszach
	southRows :: IOUArray Int Int			-- południowe bieguny w wierszach 
}

---- Maska pola.
mask :: String -> Int
mask "" = 0
mask ('S':xs) = 1 .|. (mask xs)
mask ('N':xs) = 2 .|. (mask xs)
mask ('l':xs) = 4 .|. (mask xs)
mask ('r':xs) = 8 .|. (mask xs)
mask ('d':xs) = 16 .|. (mask xs)
mask ('u':xs) = 32 .|. (mask xs)

demaskPole :: Int -> Char
demaskPole x = if x .&. 1 > 0
	then 'S'
	else if x .&. 2 > 0
	then 'N'
	else '_'

---- Przeciwny biegun.
opposite :: Int -> Int
opposite 0 = 0
opposite 1 = 2
opposite 2 = 1

---- Ten sam biegun.
samePole :: Int -> Int -> Bool
samePole x y = x .&. (mask "NS") > 0 && x .&. (mask "NS") == y .&. (mask "NS")

-- Główna funkcja programu. ---------------------------------------------------
---- Główna pętla.
solvePuzzle :: [Matrix] -> IO ()
solvePuzzle [] = return ()
solvePuzzle (top:stack) = do
	if act top > (cols top - 1, rows top - 1) -- Aktualne pole nie mieści się na mapie => wynik
	then do
		putMatrix top
		solvePuzzle stack

	else do
		result <- solvePuzzleWorker top
		solvePuzzle (result ++ stack)

---- Pojedyńczy sprawdzacz.
solvePuzzleWorker :: Matrix -> IO [Matrix]
solvePuzzleWorker matrix = do
	d <- readArray (result matrix) (act matrix)
	matrices <- if d .&. (mask "l") > 0 || d .&. (mask "u") > 0
	then (createMatrix matrix (d .&. (mask "NS"))) >>= (\result -> return [result]) -- jeśli pole to l lub u to znaczy że już wcześniej zostało ustawione.
	else (filterM (checkPos matrix) [mask "N", mask "S", mask ""]) >>= (\passed -> mapM (createMatrix matrix) passed) -- w przeciwnym przypadku wygeneruj wszystkie możliwości i sprawdź ich sens
	filterM checkFinish matrices -- przed zwróceniem sprawdź czy aby na pewno liczby na brzegu się zgadzają

---- Ustawianie magnesu.
createMatrix :: Matrix -> Int -> IO Matrix
createMatrix (Matrix (y, x) array_ width height nCols_ sCols_ nRows_ sRows_) pole = do
	array <- copy2DArray (0, 0) (w, h) array_
	nCols <- copyArray 0 w nCols_
	sCols <- copyArray 0 w sCols_
	nRows <- copyArray 0 h nRows_
	sRows <- copyArray 0 h sRows_ -- ↑ Trzeba zrobić kopię bo to wskaźniki
	d <- readArray array (y, x)
	if d .&. (mask "l") > 0 || d .&. (mask "u") > 0 -- jeśli l lub u to poprostu przeskakujemy dalej:
	then
		return (Matrix nextpos array width height nCols sCols nRows sRows)

	else do
		pos <- if d .&. (mask "r") > 0 then return (y, x + 1) -- drugi koniec magnesu
		else if d .&. (mask "d") > 0 then return (y + 1, x)
		else return (-1, -1) -- nigdy nie wystąpi

		c <- readArray array pos
		writeArray array (y, x) (d .|. pole)
		writeArray array pos (c .|. (opposite pole)) -- ustawianie magensu
		if pole .&. (mask "N") > 0 -- ustawianie wartości na brzegach
		then do
			(unsafeRead nCols x)			>>= (\a -> unsafeWrite nCols x (a - 1))
			(unsafeRead nRows y)			>>= (\a -> unsafeWrite nRows y (a - 1))
			(unsafeRead sCols (snd pos))	>>= (\a -> unsafeWrite sCols (snd pos) (a - 1))
			(unsafeRead sRows (fst pos))	>>= (\a -> unsafeWrite sRows (fst pos) (a - 1))

		else if pole .&. (mask "S") > 0
		then do
			(unsafeRead sCols x)			>>= (\a -> unsafeWrite sCols x (a - 1))
			(unsafeRead sRows y)			>>= (\a -> unsafeWrite sRows y (a - 1))
			(unsafeRead nCols (snd pos))	>>= (\a -> unsafeWrite nCols (snd pos) (a - 1))
			(unsafeRead nRows (fst pos))	>>= (\a -> unsafeWrite nRows (fst pos) (a - 1))

		else return ()

		return (Matrix nextpos array width height nCols sCols nRows sRows)

	where
		nextpos = nextPos (y, x) width
		w = width - 1
		h = height - 1

---- Sprawdzanie czy biegun może tu wejść (tak naprawdę czy cały magnes tu pasuje).
checkPos :: Matrix -> Int -> IO Bool
checkPos (Matrix (y, x) array w h nCols sCols nRows sRows) pole = do
	d <- readArray array (y, x)
	pos <- if d .&. (mask "r") > 0 -- drugi koniec magnesu
	then return (y, x + 1)
	else if d .&. (mask "d") > 0
	then return (y + 1, x)
	else return (-1, -1) -- nigdy nie wystąpi

	c <- readArray array pos
	f <- return $ fst pos
	s <- return $ snd pos
	fields <- mapM (readArray array) [(c, d) | (a, b) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], let (c, d) = (y + a, x + b), inbound h w (c, d)] -- sąsiednie pola
	if any (samePole pole) fields -- sąsiednie pola muszą być innych biegunów niż dany
	then
		return False

	else do
		fields <- mapM (readArray array) [(c, d) | (a, b) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], let (c, d) = (f + a, s + b), inbound h w (c, d)]
		if any (samePole (opposite pole)) fields -- sąsiednie pola muszą być innych biegunów niż dany
		then
			return False

		else do
			numbers <- if pole == mask "N"
			then mapM (uncurry unsafeRead) [(nCols, x), (nRows, y), (sCols, snd pos), (sRows, fst pos)]
			else if pole == mask "S"
			then mapM (uncurry unsafeRead) [(sCols, x), (sRows, y), (nCols, snd pos), (nRows, fst pos)]
			else return []

			return $ all (> 0) numbers -- sprawdzanie wartości na brzegach

-- Sprawdzanie ukończonych wierszy/kolumn
checkFinish :: Matrix -> IO Bool
checkFinish matrix = if x == 0 && y > 0
	then do
		n <- unsafeRead (northRows matrix) (y - 1)
		s <- unsafeRead (southRows matrix) (y - 1)
		return $ n == 0 && s == 0

	else if y == rows matrix - 1 && x > 0
	then do
		n <- unsafeRead (northCols matrix) (x - 1)
		s <- unsafeRead (southCols matrix) (x - 1)
		return $ n == 0 && s == 0

	else do
		return True
		nR <- unsafeRead (northRows matrix) y
		sR <- unsafeRead (southRows matrix) y
		nC <- unsafeRead (northCols matrix) x
		sC <- unsafeRead (southCols matrix) x
		return $ w >= nR && w >= sR && h >= nC && h >= sC
		
	where
		pos = act matrix
		y = fst pos
		x = snd pos
		w = cols matrix - x
		h = rows matrix - y


-- Czy punkt zawiera się w przedziale (2D, dodatnia ćwiartka)?
inbound :: Int -> Int -> (Int, Int) -> Bool
inbound h w (y, x)
   | 0 > y || y >= h || 0 > x || x >= w = False
   | otherwise = True

---- Następna pozycja.
nextPos :: (Int, Int) -> Int -> (Int, Int)
nextPos (y, x) w 
	| x + 1 == w	= (y + 1, 0)
	| otherwise		= (y, x + 1)

---- Kopiowanie tablicy.
copy2DArray s e array_ = do
	array <- unsafeFreeze array_ :: IO (UArray (Int, Int) Int)
	thaw array

copyArray s e array_ = do
	array <- Data.Array.Base.unsafeFreeze array_ :: IO (UArray Int Int)
	thaw array

-- Wczytywanie i wypisywanie. -------------------------------------------------
main :: IO ()
main = do
	putStrLn "Podaj nazwę pliku:"
	name <- getLine
	-- Wczytywanie danych
	file <- openFile name ReadMode
	rows <- hReadInt file
	cols <- hReadInt file
	matrix <- hReadMatrix file rows cols
	northCols <- hReadListInt file cols
	northRows <- hReadListInt file rows
	southCols <- hReadListInt file cols
	southRows <- hReadListInt file rows
	hClose file
	solvePuzzle [(Matrix (0, 0) matrix cols rows northCols southCols northRows southRows)]

---- Wczytywanie.

hReadInt :: Handle -> IO Int
hReadInt file = (hGetLine file) >>= (\line -> return (read line))

hReadMatrix :: Handle -> Int -> Int -> IO (IOUArray (Int, Int) Int)
hReadMatrix file rows cols =  (hGetLine file) >>= (\line -> newListArray ((0, 0), (cols - 1, rows - 1)) (map mask (concat $ read line)))

hReadListInt :: Handle -> Int -> IO (IOUArray Int Int)
hReadListInt file values = (hGetLine file) >>= (\line -> newListArray (0, values - 1) (read line))

---- Wypisywanie.

putMatrix :: Matrix -> IO ()
putMatrix matrix = do
	(getElems $ result matrix) >>= (\elems -> putRows elems 0 (cols matrix))
	putStrLn ""

putRows :: [Int] -> Int -> Int -> IO ()
putRows (e:rest) cnt max = if cnt == max
	then do
		putStrLn ""
		putRows (e:rest) 0 max

	else do 
		putChar $ demaskPole e
		putChar ' '
		putRows rest (cnt + 1) max

putRows [] _ _ = putStrLn ""

