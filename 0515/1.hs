insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = [x:y:ys] ++ (map (y:) (insert x ys))

permi :: [a] -> [[a]]
permi [] = [[]]
-- permi (x:xs) = concat (map (insert x) (permi xs))
permi (x:xs) = concatMap (insert x) (permi xs)


permi2 :: [a] -> [[a]]
permi2 [] = [[]]
permi2 (x:xs) = [zs | ys <- permi2 xs, zs <- insert x ys]

permi3 :: [a] -> [[a]]
permi3 [] = [[]]
permi3 (x:xs) = do
	ys <- permi3 xs
	insert x ys
