roots1 :: (Double, Double, Double) -> [Double] 
roots1 (a, b, c) = 
	case delta `compare` 0 of 
		EQ -> [-b/(2*a)] 
		GT -> [(-b-sqrtDelta)/(2*a) ,(-b+sqrtDelta)/(2*a)] 
		LT -> [] 
	where 
		delta = b*b - 4*a*c 
		sqrtDelta = sqrt(delta)

data Roots = No | One Double | Two (Double, Double) deriving Show 
roots2 :: (Double, Double, Double) -> Roots 
roots2 (a, b, c) =
	case delta `compare` 0 of 
		EQ -> One (-b/(2*a)) 
		GT -> Two ((-b-sqrtDelta)/(2*a),(-b+sqrtDelta)/(2*a)) 
		LT -> No 
	where 
		delta = b*b - 4*a*c 
		sqrtDelta = sqrt(delta)
