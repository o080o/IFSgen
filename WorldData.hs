module WorldData(
koch,
koch2,
koch3,
Color,
System(..),
World(..),
WorldState,
newWorld,
newSystem,
watchSystems
) where

import Control.Concurrent.STM as STM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad 
import Shapes
import Iterators

-- define some data
data WorkState = Done | Working 
data System = System	{baseShape::Shape
			,sysIter::Iterator
			,sysColor::Color
			,iter::Int
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
data Worker = Worker System ThreadId (MVar Bool)


-- Watch TChan for dirty systems to solve.
watchSystems :: TChan System -> [Worker] -> IO ()
watchSystems ch workers = do
	-- read a system from the queue
	sys <- atomically $ readTChan ch
	-- check if this system is already being solved.
	-- if so, abort and resolve. 
	mapM_ abort $ filter (isSolving sys) workers
	newWorker <- newWorker sys workers
	livingWorkers <- filterM isAlive workers
	watchSystems ch (newWorker:livingWorkers)
	
newWorker sys workers= do
	lock <- newEmptyMVar
	id <- forkIO $ exec lock $ solveSystem sys
	return (Worker sys id lock)
	where	exec lock action = do
			action
			putMVar lock True

isAlive :: Worker -> IO Bool
isAlive (Worker _ _ lock) = isEmptyMVar lock

isSolving :: System -> Worker -> Bool
isSolving sys (Worker sys' id lock) = sys==sys'

abort :: Worker -> IO ()
abort (Worker _ id lock) = killThread id

-- solves a system. may block if there is no work to do.
solveSystem :: System -> IO ()
solveSystem sys = do
	let	iterator=sysIter sys
		n=iter sys
		result=drawShapes sys
		base=baseShape sys
	iterator result n base -- call the Iterator function


-- generates a new system
newSystem :: Shape -> [Shape] -> IO System
newSystem shape rShapes = do
	s <- STM.newTVarIO []
	let sys=System	{baseShape=shape
			,sysIter=newIter
			,sysColor=(0,0,0)
			,iter=3
			,drawShapes=s}
	return sys
	where newIter = newSimpleIterator rShapes rShapes

-- generates a new world
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

