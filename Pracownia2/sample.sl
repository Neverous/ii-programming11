-- Funkcje implementujące wyrażenia warunkowe i operacje logiczne

if True x y = x;
if False x y = y;

and x y = if x y False;
or x y = if x True y;
not x = if x False True;

reverse = rev Nil where {
   rev a Nil = a;
   rev a (Cons x xs) = rev (Cons x a) xs;
}

length = len 0 where {
   len n Nil = n;
   len n (Cons x xs) = len (n+1) xs;
}
