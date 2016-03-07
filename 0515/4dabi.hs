prod xs =
	prod' xs id
	where
		prod' [] k = k 1
		prod' (0:xs) k = 0
		prod' (x:xs) k = prod' xs (\y -> k (y*x))
