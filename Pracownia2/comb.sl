-- Funkcje implementujące wyrażenia warunkowe i operacje logiczne

if True x y = x;
if False x y = y;

and x y = if x y False;
or x y = if x True y;
not x = if x False True;

-- Listy zbudowane za konstruktorów Cons/2 i Nil/0

reverse = rev Nil where {
   rev a Nil = a;
   rev a (Cons x xs) = rev (Cons x a) xs;
}

length = len 0 where {
   len n Nil = n;
   len n (Cons x xs) = len (n+1) xs;
}

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

even Nil = True;
even (Cons x Nil) = False;
even (Cons x (Cons y ys)) = even ys;

head (Cons x xs) = x;

tail (Cons x xs) = xs;
-- Funkcje wyższych rzędów i rekursja

app f g x = f (g x);

curry f x y = f (Pair x y);
uncurry g (Pair x y) = g x y;

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

foldr f c Nil = c;
foldr f c (Cons x xs) = f x (foldr f c xs);

sort = foldr ins Nil;
ins x Nil = Cons x Nil;
ins x (Cons y ys) =
   if (x < y)
      (Cons x (Cons y ys))
      (Cons y (ins x ys));

iterate f a = Cons a (iterate f (f a));

filter p Nil = Nil;
filter p (Cons x xs) = if (p x) (Cons x ys) ys where
	ys = filter p xs;

enumFrom n = Cons n (enumFrom (n+1));

primes = map head (iterate sieve (enumFrom 2)) where
	sieve (Cons x xs) = filter (\ y -> y mod x /= 0) xs;

zipWith f Nil ys = Nil;
zipWith f xs Nil = Nil;
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys);

fib = Cons 1 (Cons 1 (zipWith (\ x -> \ y -> x+y) fib (tail fib)));
