--select :: [a] -> [(a, [a])]
select [] = []
--select xs = aux (reverse xs) [] [] where
--	aux [] _ zs = zs
--	aux (x:xs) ys zs = aux xs (x:ys) ((x, foldl (\x y -> y:x) ys xs):zs)

select (x:xs) = (x,xs) : (map g $ select xs) where
						  g (a, as) = (a, x:as)

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\(x, zs) -> map (\ys -> x:ys) (perms zs)) (select xs)

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [x:zs 
	| (x, ys) <- select xs,
	  zs <- perms2 ys]

perms3 :: [a] -> [[a]]
perms3 [] = [[]]
perms3 xs = do
	(x, ys) <- select xs
	zs <- perms2 ys
	return (x:zs)
