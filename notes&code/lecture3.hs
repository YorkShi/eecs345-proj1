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
