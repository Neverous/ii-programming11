class En a where
	toEn :: a -> int
	fromEn :: int -> a

instance Enum typ (a, b) where
	toEn typ (a, b) = 5
	fromEn n = typ (a, b)

