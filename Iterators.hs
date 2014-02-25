module Iterators(
 replaceShape
,newSimpleIterator
,Iterator
) where

import Shapes
import qualified Control.Concurrent.STM as STM

type Iterator = STM.TVar [Shape] -> Int -> Shape -> IO ()

newSimpleIterator :: [Shape] -> [Shape] -> Iterator
newSimpleIterator ishapes dshapes = simpleIter_iterateN ishapes dshapes


-- iterate a single shape N times.
simpleIter_iterateN :: [Shape] -> [Shape] -> STM.TVar [Shape] -> Int -> Shape -> IO ()
simpleIter_iterateN ishapes dshapes results n s = simpleIter_iterateN' ishapes dshapes results n [s]

-- Iterate a list of shapes N times
simpleIter_iterateN' :: [Shape] -> [Shape] -> STM.TVar [Shape] -> Int -> [Shape] -> IO ()
simpleIter_iterateN' _ _ _ 0 _ = return ()
simpleIter_iterateN' ishapes dshapes results n s = do
	putStrLn $ "iter " ++ (show n)
	let (iterShapes, drawShapes) = foldl (combine) start $map (simpleIter_iterate ishapes dshapes) s
	STM.atomically $ STM.writeTVar results drawShapes
	simpleIter_iterateN' ishapes dshapes results (n-1) iterShapes
	where	combine (aci,acd) (i,d) = (i ++ aci,d ++ acd)
		start = ([],[])

-- iterate a shape once using SimpleIter
simpleIter_iterate :: [Shape] -> [Shape] -> Shape -> ([Shape],[Shape])
simpleIter_iterate ishapes dshapes s = (iterShapes,drawShapes)
	where	iterShapes = replaceShape s ishapes
		drawShapes = replaceShape s dshapes

			

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

-- perform rotate/scale/translate transformations on a shape
transformShape :: Double -> Double -> Vector -> Shape -> Shape
transformShape r s t (Point v) = Point (translate t v)
transformShape r s t (Line v1 v2) = Line (((translate t) . (scale s) . (rotate r)) v1) (((translate t) . (scale s) . (rotate r)) v2)
transformShape r s t (Circle v1 radius) = Circle (((translate t) . (scale s) . (rotate r)) v1) s

--take a Shape and replace with a list of Shapes
replaceShape ::  Shape -> [Shape] -> [Shape]

replaceShape (Point (bx,by)) replaceShapes = shapes
	where	t = (bx, by)
		shapes = map (transformShape 0 1 t) replaceShapes 
replaceShape (Line (bx1, by1) (bx2, by2)) replaceShapes = shapes
	where	dx = bx2-bx1
		dy = by2-by1
		dist = sqrt $ dx * dx + dy * dy
		r = atan2 dy dx
		s = dist
		t = (bx1, by1)
		shapes = map (transformShape r s t) replaceShapes 

replaceShape (Circle (bh, bk) br) replaceShapes = shapes
	where	r = 0
		s = br
		t = (bh, bk)
		shapes = map (transformShape r s t) replaceShapes 
