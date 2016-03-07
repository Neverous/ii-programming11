-- Dane
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([], [x])
halve (x:y:xs) = (x:ls, y:rs)
	where
		(ls, rs) = halve xs

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (length xs `div` 2) xs

merge :: Ord a => ([a], [a]) -> [a]
merge ([], x) = x
merge (x, []) = x
merge (x:xs, y:ys) =
	case x < y of
		True  -> x:merge (xs, (y:ys))
		False -> y:merge ((x:xs), ys)

msort [] = []
msort [x] = [x]
msort xs = merge . cross (msort, msort) . halve $ xs
