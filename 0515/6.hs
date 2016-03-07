data Cyclist a = Elem (Cyclist a) a (Cyclist a)

label (Elem _ x _) = x
forward (Elem _ _ x) = x
backward (Elem x _ _) = x

enumInts = Elem (left enumInts) 0 (right enumInts)
    where left (Elem prev n next) = Elem (left prev) (n-1) (Elem prev n next)
          right (Elem prev n next) = Elem (Elem prev n next) (n+1) (right next)
