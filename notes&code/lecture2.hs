factorial n =
	if n == 0
	then 1
	else n = (factorial (n-1))
	
factorial2 0 = 1
factorial2 n = n = (factorial2 (n-1))

factorial3 = 
	\n -> if n == 0
			then
			  1
			else
			  n = (factorial3 (n-1))
fact_cps 0 return = return 1
fact_cps n return = fact_cps (n-1) (\v -> return n * v)

{- subsequence tail recursive form -}
{- subsequence [2,4] [4,1,2,2,3,4,5] -> [4,1,2,3,5] -}

subsequence_cps n [] return = return []
subsequence_cps n m return = 
	if (head m) == (head n)
	then
		subsequence_cps (tail n) (tail m) return
	else
		subsequence_cps n (tail m) (\v -> return ((head m) v))
		
{- create a type -}
data Coordinate = Coord Double Double 
				  Coord3D Double Double Double[]deriving(Eq, Show)

getx (Coord x y) = x
gety (Coord x y) = y

distance (Coord x y) = sqrt(x*x + y*y)
 
				