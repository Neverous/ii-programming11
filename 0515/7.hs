data Cyclist a = Elem (Cyclist a) a (Cyclist a)

label (Elem _ x _) = x
forward (Elem _ _ x) = x
backward (Elem x _ _) = x

enumInts = Elem (left enumInts) 0 (right enumInts)
    where left (Elem prev n next) = Elem (left prev) (n-1) (Elem prev n next)
          right (Elem prev n next) = Elem (Elem prev n next) (n+1) (right next)

newtype Cyc a b = Cyc (Cyclist a -> (b, Cyclist a))
instance Monad (Cyc a) where
    (>>=) (Cyc st) f = Cyc (\m -> let (val, li) = st m
                                      Cyc st2 = f val
                                  in st2 li)
    return val = Cyc (\x -> (val, x))

runCyc :: Cyclist a -> Cyc a b -> b
runCyc a (Cyc ob) = fst (ob a)

fwd :: Cyc a ()
fwd = Cyc (\x -> ((), forward x))

bkw :: Cyc a ()
bkw = Cyc (\x -> ((), backward x))

lbl :: Cyc a a
lbl = Cyc (\x -> (label x, x))

example :: Integer
example = runCyc enumInts (do
	bkw
	bkw
	bkw
	bkw
	x <- lbl
	fwd
	fwd
	y <- lbl
	fwd
	z <- lbl
	return (x + y + z))
