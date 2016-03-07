-- (f . g) x = f (g x)
-- flip f a b = f b a
-- curry f a b = f (a,b)
f (a, b) = 2^a + 3^b
swap (a, b) = (b, a)
