module Functions(
replaceIterate
) where

import Shapes

--kochIter Line Vertex(x1, y1) Vertex(x2, y2) = [Line Vertex(x1,y1) mid1 , Line mid1 apex, Line apex mid2, Line mid2, Vertex(x2, y2)]
--	where	apex = Vertex(
--		mid1 = Vertex(x1 + (x2-x1)/3, y1 + (y2-y1)/3)
--		mid1 = Vertex(x2 - (x2-x1)/3, y2 - (y2-y1)/3)

scale :: Double -> Vector -> Vector
scale s (x, y) = (x*s, y*s) 

translate :: Vector -> Vector -> Vector
translate (tx, ty) (x,y)  = (x+tx, y+ty)

rotate :: Double -> Vector -> Vector
rotate r (x, y) = ((x*cos r) - (y * sin r ), (x * sin r) + (y * cos r))

--take a Shape and replace with a list of Shapes
replaceIterate ::  Shape -> [Shape] -> Shape -> ([Shape],[Shape])

replaceIterate (Point (bx,by)) replaceShapes s = (iterShapes, drawShapes)
	where	t = (bx, by)
		transform (Point v) = Point (translate t v)
		drawShapes = map transform $ filter ( not . (isPoint)) replaceShapes 
		iterShapes = map transform $ filter (isPoint) replaceShapes 

replaceIterate (Line (bx1, by1) (bx2, by2)) replaceShapes s = (iterShapes, drawShapes)
	where	dist = sqrt $ (bx2-bx1) * (bx2-bx1) + (by2-by1) * (by2-by1)
		r = acos $ (by2-by1)/dist
		s = dist
		t = (bx1, by1)
		transform (Line v1 v2) = Line (((translate t) . (scale s) . (rotate r)) v1) (((translate t) . (scale s) . (rotate r)) v2)
		drawShapes = map transform  $ filter ( not . (isLine)) replaceShapes 
		iterShapes = map transform $ filter (isLine) replaceShapes 

replaceIterate (Circle (bh, bk) br) replaceShapes s = (iterShapes, drawShapes)
	where	r = 1
		s = r
		t = (bh, bk)
		transform (Circle v1 radius) = Circle (((translate t) . (scale s) . (rotate r)) v1) s
		drawShapes = map transform  $ filter ( not . (isCircle)) replaceShapes 
		iterShapes = map transform $ filter (isCircle) replaceShapes 









-- [Shape] -> (Shape -> [Shape] -> [Shape]) -> [Shape]
