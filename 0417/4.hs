fib n = acufib n (1, 1) where
	acufib n (a, b)
		| n == 0    = a
		| otherwise = acufib (n - 1) (b, a + b)

fib2 = 1 : 1 : zipWith (+) fib2 (tail fib2)
