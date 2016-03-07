-- Dane
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

merge_unique :: Ord a => [a] -> [a] -> [a]
merge_unique [] x = x
merge_unique x [] = x
merge_unique (x:xs) (y:ys)
	| x == y = x:merge_unique xs ys
	| x < y  = x:merge_unique xs (y:ys)
	| x > y  = y:merge_unique (x:xs) ys

d235 = 1:merge_unique [2 * x | x <- d235] (merge_unique [3 * x | x <- d235] [5 * x | x <- d235]) 
--d235 = 1:merge_unique (map (\x -> 2 * x) d235) (merge_unique (map (\x -> 3 * x) d235) (map (\x -> 5 * x) d235))
