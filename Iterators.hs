module Iterators(
 newKoch
,newIFS
,newKoch'
,newIFS'
,solveKoch
,solveIFS
) where

import Shapes
import System
import WorldData
import qualified Control.Concurrent.STM as STM

-- generate a new Koch curce IFS
newKoch' :: STM.TVar [Shape] -> Vector -> Vector -> System
newKoch' s p1 p2 = System	{controlPoints=[p1,p2]
				,solveFunc=solveKoch
				,baseShapes=asLines
				,iter=3
				,drawShapes=s}

newKoch :: Vector -> Vector -> IO System
newKoch p1 p2 = do
	s <- STM.newTVarIO []
	return $ newKoch' s p1 p2 

-- generate a new IFS
newIFS' :: STM.TVar [Shape] -> Shape -> [Shape] -> System
newIFS' s shape rShapes = System	{controlPoints=baseCP
					,solveFunc=solveIFS rShapes rShapes
					,baseShapes=asLines
					,iter=3
					,drawShapes=s}
	where	baseCP = vertices shape
		replaceCP = concat $ map (vertices) rShapes

newIFS :: Shape -> [Shape] -> IO System
newIFS shape rShapes = do
	s <- STM.newTVarIO []
	return $ newIFS' s shape rShapes

solveKoch :: System -> IO ([Shape])
solveKoch = solveIFS kochi kochd

solveIFS :: [Shape] -> [Shape] -> System -> IO ([Shape])
solveIFS ishapes dshapes sys = simpleIter_iterateN' ishapes dshapes results n base []
	where	n = iter sys
		base = ((baseShapes sys) . controlPoints) sys
		results = drawShapes sys
-- Iterate a list of shapes N times
simpleIter_iterateN' :: [Shape] -> [Shape] -> STM.TVar [Shape] -> Int -> [Shape] -> [Shape] -> IO ([Shape])
simpleIter_iterateN' _ _ _ 0 _ prevDraw = do 
	putStrLn "iter 0"
	return (prevDraw)
simpleIter_iterateN' ishapes dshapes results n s prevDraw = do
	putStrLn $ "iter " ++ (show n)
	let (iterShapes, drawShapes) = foldl (combine) start $map (simpleIter_iterate ishapes dshapes) s
	STM.atomically $ STM.writeTVar results (drawShapes ++ prevDraw)
	simpleIter_iterateN' ishapes dshapes results (n-1) iterShapes (drawShapes ++ prevDraw)
	return ishapes
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

--take a Shape and replace with a list of Shapes
replaceShape ::  Shape -> [Shape] -> [Shape]
replaceShape base rShapes =  map (transform r s t) rShapes
	where	r = rotation base
		s = scale base
		t = offset base
