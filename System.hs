module System(
System(..),
watchSystems
) where

import Control.Concurrent.STM as STM
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad 
import Shapes

-- define some data
type Solver = System -> IO ([Shape])
type DrawOp = IO ()
data System = System	{drawShapes::STM.TVar [Shape]
			,solveFunc::Solver
			,baseShapes::[Vector]->[Shape]
			,iter::Int
			,controlPoints :: [Vector]}
			
instance Eq System where
	x == y = (drawShapes x) == (drawShapes y)

--------------------------
data Worker = Worker System ThreadId (MVar [Shape])

-- Watch TChan for dirty systems to solve.
watchSystems :: TChan System -> [Worker] -> IO ()
watchSystems ch workers = do
	-- read a system from the queue
	sys <- atomically $ readTChan ch
	-- check if this system is already being solved.
	-- if so, abort and resolve. 
	let sysworkers = filter (isSolving sys) workers
	mapM_ abort sysworkers
	livingWorkers	<- filterM isWorking workers
	finishedWorkers	<- filterM (isFinished) sysworkers
	worker <- newWorker sys
	watchSystems ch (worker:livingWorkers)

startWork :: System -> MVar [Shape] -> IO ([Shape]) -> IO Worker
startWork sys lock action = do
	id <- forkIO $ exec lock action
	return $ Worker sys id lock
	where	exec lock action = do
		result <- action
		putStrLn "done work."
		-- force evaluation ?
		putMVar lock result
-- starts a new worker to solve the system
newWorker :: System -> IO Worker
newWorker sys = do
	lock <- newEmptyMVar
	startWork sys lock (solveSystem sys)

isWorking :: Worker -> IO Bool
isWorking (Worker _ _ lock) = isEmptyMVar lock

isFinished :: Worker -> IO Bool
isFinished worker = do
	working <- isWorking worker
	return (not working)

isSolving :: System -> Worker -> Bool
isSolving sys (Worker sys' id lock) =	sys==sys'

abort :: Worker -> IO ()
abort (Worker _ id lock) = killThread id

-- solves a system. may block if there is no work to do.
solveSystem :: System -> IO ([Shape])
solveSystem sys = do
	let	solve=solveFunc sys
		n=iter sys
		result=drawShapes sys
	if n < 0 then return []
	else solve sys -- call the Iterator function and demand a result, not a thunk!!
