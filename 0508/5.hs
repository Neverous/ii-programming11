length = foldr (\_ y -> y + 1) 0
length = foldl (\x _ -> x + 1) 0
(++) = flip $ foldr (:)
concat = foldr (++) []
reverse = foldl (flip (:)) []
sum = foldl (+) 0
