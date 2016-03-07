prod xs = 
	case res of
		Nothing -> 0
		Just n -> n
	where
		res = f (Just 1) xs
		f = foldr (\ n p ->
				case p of
					Nothing -> Nothing
					Just 0 -> Nothing
					Just w -> Just (w * n))

prod2 = foldr (\n p -> if n==0 then 0 else n*p) 1
