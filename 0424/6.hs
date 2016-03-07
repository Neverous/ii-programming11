class Monoid a where
	(***) :: a -> a -> a
	e :: a
	infixl 6 ***

(^^^) :: Monoid a => a -> Integer -> a
(^^^) a 0 = e
(^^^) a n
	| odd n = a *** h *** h
	| otherwise = h *** h
	where
		h = a ^^^ (n `div` 2)
infixr 7 ^^^

data Mtx2x2 a = Mtx2x2 a a a a

instance Num a => Monoid (Mtx2x2 a) where 
	e = Mtx2x2 1 0 0 1
	(***) (Mtx2x2 a11 a12 a21 a22) (Mtx2x2 b11 b12 b21 b22) = Mtx2x2 c11 c12 c21 c22 
		where 
			c11 = a11 * b11 + a12 * b21 
			c12 = a11 * b12 + a12 * b22 
			c21 = a21 * b11 + a22 * b21 
			c22 = a21 * b12 + a22 * b22 

gimmeh (Mtx2x2 a b c d) = show a ++ show b ++ show c ++ show d
