module InputHandlers(
setWindowCallbacks,
InputMsg(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Control.Concurrent.STM as STM

import WorldData
import Shapes
import Util
import Iterators

-- Input messages
data InputMsg 	=WinResize Int Int
		|WinRefresh
		|WinClose
		|LBtnUp Int Int
		|LBtnDwn Int Int
		|RBtnUp Int Int
		|RBtnDwn Int Int
		|Mouse Int Int
		|KeyUp Int
		|KeyDwn Int

dragThresh = 5

-- GLFW handlers!
-- type declarations
-- window callbacks:
errorCallback 		:: GLFW.Error -> String -> IO ()
windowSizeCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
windowRefreshCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
-- input callbacks:
mouseButtonCallback	:: STM.TChan System -> WorldState -> GLFW.Window -> GLFW.MouseButton ->GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback	:: STM.TChan System -> WorldState -> GLFW.Window -> Double -> Double -> IO ()
keyCallback		:: STM.TChan System -> WorldState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()

-- Define handlers. Where the Magic happens!
errorCallback err str = putStrLn str

-- window related callbacks
windowSizeCallback ch _ win w h = resize w h

windowCloseCallback ch _ win = GLFW.destroyWindow win

windowRefreshCallback ch _ win = STM.atomically $ STM.writeTChan ch WinRefresh

-- mouse related callbacks
-- mouse press
mouseButtonCallback ch worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = STM.atomically $ do
	world <- STM.readTVar worldState
	let	(x,y) = lastPos world
		(newsys,sel,pnt) = pickSys x y world
	STM.writeTVar worldState $ world	{mouseDown=True
						,mouseDragging=False
						,systems=newsys
						,selectedSys=sel
						,selectedPoint=pnt
						,firstPos=lastPos world}


-- mouse release
mouseButtonCallback ch worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = STM.atomically $ do
	world <- STM.readTVar worldState
	STM.writeTVar worldState $ world	{mouseDown=False
						,mouseDragging=False
						,selectedPoint=Nothing}
-- catch-all mouseButton pattern
mouseButtonCallback ch worldState win _ _ _ = return ()

-- mouse move
cursorPosCallback ch worldState win x y = STM.atomically $ do
		world <- STM.readTVar worldState
		-- test for mosue drag conditions
		if mouseDown world && (mouseDragging world || distance (firstPos world) (x,y)>dragThresh) then do
			let	sel = selectedSys world
				pnt = selectedPoint world
				sel' = transformSys sel pnt
			case pnt of
				Just _ -> do
					STM.writeTVar worldState $ world	{mouseDragging=True
										,selectedSys=sel'
										,lastPos=(x,y)}
					STM.writeTChan ch sel'
				Nothing -> return ()
		else STM.writeTVar worldState world {lastPos=(x,y)}
	where	transformSys sys Nothing = sys
		transformSys sys (Just pnt) = snapTo x y sys pnt
-- keyboard related callbacks

-- Delete selected system on DELETE key
keyCallback ch worldState _ GLFW.Key'Delete _ GLFW.KeyState'Pressed _ = STM.atomically $ do
	world <- STM.readTVar worldState
	let	newsys = systems world
	if length newsys > 1 then
		STM.writeTVar worldState $ world	{systems=tail newsys,selectedSys=head newsys}
	else
		return ()
-- Add a new system on SPACE key
keyCallback ch worldState _ GLFW.Key'Space _ GLFW.KeyState'Pressed _ = do
	STM.atomically $ do
		world <- STM.readTVar worldState
		s<-STM.newTVar []
		let	(x,y) = lastPos world
			sel = selectedSys world
			sys = newKoch' s (x,y) (x+100,y) -- make a new system while in IO
		STM.writeTVar worldState $ world	{systems=sel:(systems world)
							,selectedSys=sys}
		STM.writeTChan ch sys

-- increase iterations on +/= key
keyCallback ch worldState _ GLFW.Key'Equal _ GLFW.KeyState'Pressed _ = STM.atomically $ do
		world <- STM.readTVar worldState
		let	sel = selectedSys world
			sel' = sel { iter=(iter sel) + 1 }
		STM.writeTVar worldState $ world	{selectedSys=sel'}
		STM.writeTChan ch sel'
-- decrease iterations on -/_ key
keyCallback ch worldState _ GLFW.Key'Minus _ GLFW.KeyState'Pressed _ = STM.atomically $ do
		world <- STM.readTVar worldState
		let	sel = selectedSys world
			sel' = if (iter sel)>0 then sel { iter=(iter sel) - 1 } else sel
		STM.writeTVar worldState $ world	{selectedSys=sel'}
		STM.writeTChan ch sel'

-- catch-all keyboard callback
keyCallback ch worldState win key sc keyState mod = putStrLn $ "key" ++ (show key) ++ " " ++ (show sc) ++ " " ++ (show keyState) ++ " " ++ (show mod)
	
---------- helper functions-------------	
--
-- pick a system to select out of the world. 
pickSys :: Double -> Double -> World -> ([System],System,Maybe Int)
pickSys x y world =	let	oldsel = selectedSys world
				oldsys = systems world
				(newsel,pnt) = pick selectable
				newsys = filter (newsel /=) (oldsel:oldsys)
			in (newsys,newsel,pnt)
	where	selectable = filter (canSelect)  ((selectedSys world):(systems world)) -- returns a list of possible selectable systems
		pick [] = (selectedSys world,Nothing)
		pick (s:_) = (s,selPoint s)
		selPoint s = foldl (\acc (i,p)->if p `closeTo` (x,y) then Just i else acc) Nothing (zip [0..] (controlPoints s))
		closePoints s = filter (closeTo (x,y)) (controlPoints s)
		canSelect s = (not . null) (closePoints s)

-- Snap system  point to x,y
snapTo :: Double -> Double -> System -> Int -> System
snapTo x y sys n = sys { controlPoints=newPoints }
	where	points = controlPoints sys
		newPoints = (take n points)++(x,y):(drop (n+1) points)

-- helper functions for selecting shapes
closeToGen :: (Num a, Ord a) => a -> (a,a) -> (a,a) -> Bool
closeToGen d (x,y) (x',y') = ((x-d) < x' && x' < (x+d)) && ((y-d) < y' && y' < (y+d))
closeTo = (closeToGen 50.0)







------------------------ Register all callbacks
setWindowCallbacks :: STM.TChan InputMsg -> STM.TChan System -> WorldState -> GLFW.Window -> IO ()
setWindowCallbacks winch ch world win = do
	GLFW.setErrorCallback		   $ Just $ errorCallback
	GLFW.setWindowSizeCallback	win $ Just $ windowSizeCallback winch world
	GLFW.setWindowCloseCallback	win $ Just $ windowCloseCallback winch world
	GLFW.setWindowRefreshCallback	win $ Just $ windowRefreshCallback winch world
	GLFW.setMouseButtonCallback	win $ Just $ mouseButtonCallback ch world
	GLFW.setCursorPosCallback	win $ Just $ cursorPosCallback ch world
	GLFW.setKeyCallback		win $ Just $ keyCallback ch world
