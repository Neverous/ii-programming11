newtype FSet a = FSet (a -> Bool)
empty :: FSet a
empty = FSet (\_ -> False)
singleton x = FSet (\w -> w == x)
fromList l = FSet (\w -> w `elem` l)
union (FSet f) (FSet g) = FSet (\w -> f(w) || g(w))
intersection (FSet f) (FSet g) = FSet (\w -> f(w) && g(w))
member a (FSet f) = f(a)

