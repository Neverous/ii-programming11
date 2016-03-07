class Monoid a where
	(***) :: a -> a -> a
	e :: a
	infixl 6 ***

instance Monoid Integer where
	e = 1
	(***) a b = a * b `mod` 9876543210

(^^^) :: Monoid a => a -> Integer -> a
(^^^) a 0 = e
(^^^) a n
	| odd n = a *** h *** h
	| otherwise = h *** h
	where
		h = a ^^^ (n `div` 2)
infixr 7 ^^^
