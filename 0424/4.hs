merge :: Ord a => ([a], [a]) -> [a]
merge ([], x) = x
merge (x, []) = x
merge (x:xs, y:ys) =
	case x < y of
		True  -> x:merge (xs, (y:ys))
		False -> y:merge ((x:xs), ys)

msortn :: Ord a => [a] -> Int -> [a] 
msortn [] _ = []
msortn _ 0 = [] 
msortn (x:xs) 1 = [x] 
msortn xs n = merge (msortn xs mid, (msortn rest (n-mid))) 
	where
		mid  = n `div` 2 
		rest   = drop mid xs
