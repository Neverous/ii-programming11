data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList xs = let (first,last) = makeList last xs first in first 

makeList prev [] next = (next,prev) 
makeList prev (x:xs) next = let this = Elem prev x rest 
                                (rest,last) = makeList this xs next 
                                in (this,last) 

label (Elem _ x _) = x
forward (Elem _ _ x) = x
backward (Elem x _ _) = x
