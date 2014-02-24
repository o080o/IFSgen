module WorldData(
koch,
koch2,
koch3,
Color,
System(..),
World(..),
WorldState,
newWorld,
newSystem
) where

import Control.Concurrent.STM as STM
import Control.Concurrent.MVar
import Shapes
import Iterators

-- define some data
data WorkState = Done | Working 
data System = System	{baseShape::Shape
			,sysIter::Iterator
			,sysColor::Color
			,iter::Int
			,state::MVar WorkState
			,drawShapes::STM.TVar [Shape]}
instance Eq System where
	x == y = (drawShapes x) == (drawShapes y)



data World = World	{systems::[System]
			,selectedSys::System
			,selectedPoint::Maybe Int
			,mouseDragging::Bool
			,mouseDown::Bool
			,firstPos::(Double,Double)
			,lastPos::(Double,Double)}
type WorldState = STM.TVar World



newSystem :: Shape -> [Shape] -> IO System
newSystem shape rShapes = do
	s <- STM.newTVarIO []
	st <- newEmptyMVar
	return System	{baseShape=shape
			,sysIter=newIter
			,sysColor=(0,0,0)
			,iter=3
			,state=st
			,drawShapes=s}
	where newIter = newSimpleIterator rShapes rShapes

newWorld :: [System] -> IO WorldState
newWorld [] = do
	defaultSys <- newSystem (Point (0,0)) [Point (0,0)]
	STM.newTVarIO World	{systems=[]
				,selectedSys=defaultSys
				,selectedPoint=Nothing
				,mouseDragging=False
				,mouseDown=False
				,firstPos=(0,0)
				,lastPos=(0,0)}
newWorld (s:xs) = STM.newTVarIO World	{systems=s:xs
					,selectedSys=s
					,selectedPoint=Nothing
					,mouseDragging=False
					,mouseDown=False
					,firstPos=(0,0)
					,lastPos=(0,0)}

-- some default system shapes.
koch =		[Line (0,0) (1/3,0) 
		,Line (1/3,0) (0.5,0.5-(1/3))
		,Line (0.5,0.5-(1/3)) (2/3,0)
		,Line (2/3,0) (1,0)]

koch2 =		[Line (0,0) (1/3,0) 
		,Line (0.5,0.5-(1/3)) (1/3,0)
		,Line (2/3,0) (0.5,0.5-(1/3)) 
		,Line (2/3,0) (1,0)]

koch3 =		[Line (1/3,0) (0,0)
		,Line (0.5,0.5-(1/3)) (1/3,0)
		,Line (2/3,0) (0.5,0.5-(1/3)) 
		,Line (1,0) (2/3,0)]

