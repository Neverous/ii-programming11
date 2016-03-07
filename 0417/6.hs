import Data.List
import Data.Char

integerToString :: Integer -> String 
integerToString n 
	| n == 0    = "0"
	| n < 0     = '-' : g 
	| otherwise = g 
	where 
		g   = map intToDigit (reverse (unfoldr x (fromEnum (abs n))))
		x 0 = Nothing 
		x n = Just (n `mod` 10, n `div` 10) 
