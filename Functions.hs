module Functions(
iterateSystem,
replaceShape
) where

import Shapes

--kochIter Line Vertex(x1, y1) Vertex(x2, y2) = [Line Vertex(x1,y1) mid1 , Line mid1 apex, Line apex mid2, Line mid2, Vertex(x2, y2)]
--	where	apex = Vertex(
--		mid1 = Vertex(x1 + (x2-x1)/3, y1 + (y2-y1)/3)
--		mid1 = Vertex(x2 - (x2-x1)/3, y2 - (y2-y1)/3)
--

atan2' :: Double -> Double -> Double
atan2' y 0
	| y >  0 = pi/2
	| y <  0 = pi/(-2)
	| otherwise = 0
atan2' y x
	| x >  0 = atan y/x
	| y >= 0 = pi + (atan y/x)
	| y <  0 = (atan y/x) - pi
	


scale :: Double -> Vector -> Vector
scale s (x, y) = (x*s, y*s) 

translate :: Vector -> Vector -> Vector
translate (tx, ty) (x,y)  = (x+tx, y+ty)

rotate :: Double -> Vector -> Vector
rotate r (x, y) = ((x*cos r) - (y * sin r ), (x * sin r) + (y * cos r))

-- iterate n times and return a list drawable shapes
iterateSystem :: Shape -> [Shape] -> Int -> [Shape]
iterateSystem base shapes 0 = [base]
iterateSystem base shapes n = let (iterShapes, drawShapes) = replaceShape base shapes in concat $ [concat (map (\s -> iterateSystem s shapes (n-1)) iterShapes), drawShapes]

--take a Shape and replace with a list of Shapes
replaceShape ::  Shape -> [Shape] -> ([Shape],[Shape])

replaceShape (Point (bx,by)) replaceShapes = (iterShapes, drawShapes)
	where	t = (bx, by)
		transform (Point v) = Point (translate t v)
		drawShapes = map transform $ filter ( not . (isPoint)) replaceShapes 
		iterShapes = map transform $ filter (isPoint) replaceShapes 

replaceShape (Line (bx1, by1) (bx2, by2)) replaceShapes = (iterShapes, drawShapes)
	where	dx = bx2-bx1
		dy = by2-by1
		dist = sqrt $ dx * dx + dy * dy
		r = atan2 dy dx
		s = dist
		t = (bx1, by1)
		transform (Line v1 v2) = Line (((translate t) . (scale s) . (rotate r)) v1) (((translate t) . (scale s) . (rotate r)) v2)
		drawShapes = map transform  $ filter ( not . (isLine)) replaceShapes 
		iterShapes = map transform $ filter (isLine) replaceShapes 

replaceShape (Circle (bh, bk) br) replaceShapes = (iterShapes, drawShapes)
	where	r = 1
		s = br
		t = (bh, bk)
		transform (Circle v1 radius) = Circle (((translate t) . (scale s) . (rotate r)) v1) s
		drawShapes = map transform  $ filter ( not . (isCircle)) replaceShapes 
		iterShapes = map transform $ filter (isCircle) replaceShapes 









-- [Shape] -> (Shape -> [Shape] -> [Shape]) -> [Shape]
