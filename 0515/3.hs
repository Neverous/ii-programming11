sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = concatMap (\y -> [x:y, y]) (sublists xs)

sublists2 :: [a] -> [[a]]
sublists2 [] = [[]]
sublists2 (x:xs) = [subs | ys <- sublists2 xs, subs <- [x:ys, ys]]

sublists3 :: [a] -> [[a]]
sublists3 [] = [[]]
sublists3 (x:xs) = do
	ys <- sublists3 xs
	[x:ys, ys]
