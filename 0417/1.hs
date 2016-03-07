rev = aux [] where
	aux ys [] = ys
	aux ys (x:xs) = aux (x:ys) xs
