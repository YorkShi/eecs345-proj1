data Coordinate = Coord Double Double
				  Coord3D Double Double Double
				  Coord4D Double Double Double Double deriving (Show, Eq)

distance (Coord x y) = sqrt(x*x + y*y)
distance (Coord3D x y z) = sqrt(x*x + y*y + z*z)
distance (Coord4D w x y z) = sqrt(w*w + x*x + y*y + z*z)

-- add two coordinate values
add (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)
add (Coord x1 y1 z1) (Coord x2 y2) = Coord3D (x1 + x2) (y1 + y2) z1
add (Coord x1 y1) (Coord x2 y2 z2) = Coord3D (x1 + x2) (y1 + y2) z2

{-
c_getx (Coord x _) = x
c_gety (Coord _ y) = y

add :: Coordinate -> Coordinate -> Coordinate
add c1 c2 = Coord ((c_getx c1) + (c_getx c2)) ((c_gety c1) + (c_gety c2))
-}

data GenericCoordinate a = GCoord a a deriving (Show Eq)

mydivide x y = x / y
mydivide2 x = (\y -> x/y)

mysub x y z = (x - y) - z
mysub2 x = (\y -> (\z -> (x - y) - z))

{- Create a data type that is a rational but also keeps track of whether we ever
tried to make the denominator 0
 -}
data Value a = NaN | Value a deriving (Show, Eq)

-- the divide for the value type
(//) NaN _ = NaN
(//) _ NaN = NaN
(//) (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

-- the divide for the Value type and a function, applies the function to the internal type
-- of value and returns a new value
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just x) f = f x

-- vreturn creates a Value type given an element
vreturn :: a -> Maybe a
vreturn x = Just x

-- the divide using bind and vreturn
(//) vx vy =
	bind vx (\x ->
		bind vy (\y -> if y == 0 then Nothing else vreturn (x/y)))
