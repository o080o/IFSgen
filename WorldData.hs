module WorldData(
kochi,
kochd,
koch2,
koch3,
Color,
System(..),
World(..),
WorldState,
newWorld,
) where

import Control.Concurrent.STM as STM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad 

import Shapes
import System

-- define some data
type Color = (Double,Double,Double)
data World = World	{systems::[System]
			,selectedSys::System
			,selectedPoint::Maybe Int
			,mouseDragging::Bool
			,mouseDown::Bool
			,firstPos::(Double,Double)
			,lastPos::(Double,Double)}
type WorldState = STM.TVar World

-- generates a new world
newWorld :: [System] -> IO WorldState
newWorld [] = do
	s <- STM.newTVarIO []
	let defaultSys = System {System.controlPoints=[(0,0)]
				,solveFunc=(\s->return ([]))
				,baseShapes=asPoints
				,iter=0
				,drawShapes=s}
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
kochi =		[newLine (0,0) (1/3,0) 
		,newLine (1/3,0) (0.5,0.5-(1/3))
		,newLine (0.5,0.5-(1/3)) (2/3,0)
		,newLine (2/3,0) (1,0)]
kochd =		[newLine (0,0) (1/3,0) 
		,newPoly [(1/3,0),(0.5,0.5-(1/3)),(2/3,0)]
		,newLine (2/3,0) (1,0)]

koch2 =		[newLine (0,0) (1/3,0) 
		,newLine (0.5,0.5-(1/3)) (1/3,0)
		,newLine (2/3,0) (0.5,0.5-(1/3)) 
		,newLine (2/3,0) (1,0)]

koch3 =		[newLine (1/3,0) (0,0)
		,newLine (0.5,0.5-(1/3)) (1/3,0)
		,newLine (2/3,0) (0.5,0.5-(1/3)) 
		,newLine (1,0) (2/3,0)]

