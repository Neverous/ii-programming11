ssm :: Ord a => [a] -> [a]
ssm [] = []
ssm (x:xs) = reverse (foldr maxlist [x] (reverse xs))
	where maxlist new (y:ys) | new > y = new:y:ys
							 | otherwise = y:ys

ssm2 [] = []
ssm2 (x:xs) = reverse $ foldl maxlist [x] xs
	where maxlist (y:ys) new | new > y = new:y:ys
							 | otherwise = y:ys

ssm3 xs = foldr aux [] xs
	where aux a [] = [a]
		  aux a (h:hs)
			| a < h = a:h:hs
			| otherwise = aux a hs
