module InputHandlers(
setWindowCallbacks,
InputMsg(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Control.Concurrent.STM as STM
--import Data.StateVar hiding ($=)
import WorldData
import Shapes

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
errorCallback 		:: GLFW.Error -> String -> IO ()
windowSizeCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
windowRefreshCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
mouseButtonCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> GLFW.MouseButton ->GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback	:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> Double -> Double -> IO ()
keyCallback		:: STM.TChan InputMsg -> WorldState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()

-- Define handlers. Where the Magic happens!
errorCallback err str = putStrLn str
-- window related callbacks
windowSizeCallback tc _ win w h = resize w h
windowCloseCallback tc _ win = GLFW.destroyWindow win
windowRefreshCallback tc _ win = STM.atomically $ STM.writeTChan tc WinRefresh
-- mouse related callbacks
-- mouse press
mouseButtonCallback tc worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = do 
	putStrLn "."
	STM.atomically $ do
		world <- STM.readTVar worldState
		(x,y) <- return $ lastPos world
		-- test if we clicked on a system...........
		case selectableSystems x y world of
			[] -> STM.writeTVar worldState world {mouseDown=True, mouseDragging=False, selectedPoint=Nothing}
			-- change mouse state and change selected system (if any)
			s:xs ->	let	oldsel = selectedSys world
					newsys = filter (s /=) $ oldsel:(systems world)
	
					pnt = selectPoint x y (baseShape s)
				in STM.writeTVar worldState $ world {mouseDown=True, mouseDragging=False, systems=newsys,selectedSys=s,selectedPoint=Just pnt,firstPos=lastPos world}

	where	selectableSystems x y world = filter (canSelectSys x y)  ((selectedSys world):(systems world))

-- mouse release
mouseButtonCallback tc worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = STM.atomically $ do
	world <- STM.readTVar worldState
	STM.writeTVar worldState $ world {mouseDown=False, mouseDragging=False, selectedPoint=Nothing}
-- catch-all mouseButton pattern
mouseButtonCallback tc worldState win _ _ _ = return ()

-- mouse move
cursorPosCallback tc worldState win x y = STM.atomically $ do
	world <- STM.readTVar worldState
	if mouseDown world && (mouseDragging world || distance (firstPos world) (x,y)>dragThresh) then
		case selectedPoint world of
			Just pnt ->	let newsys = snapTo x y (selectedSys world) pnt
					in STM.writeTVar worldState $ world { mouseDragging=True, selectedSys=newsys, lastPos=(x,y) }
			Nothing -> STM.writeTVar worldState world {lastPos=(x,y)}
	else STM.writeTVar worldState world {lastPos=(x,y)}
-- keyboard related callbacks
keyCallback tc worldState win key sc keyState mod = putStrLn $ "key" ++ (show key) ++ (show sc) ++ (show keyState)
	
---------- helper functions-------------	
-- resize function
resize :: Int -> Int -> IO ()
resize w h = do
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
	GL.matrixMode $= GL.Projection
	GL.loadIdentity
	GL.ortho 0 (realToFrac w) (realToFrac h) 0 (-1) 1
	--GL.perspective 45 (fromIntegral w / fromIntegral h) 1 100
	GL.matrixMode $= GL.Modelview 0

-- return the distance between 2 points
distance :: (Floating a) => (a,a) -> (a,a) -> a
distance (x1,y1) (x2,y2) = sqrt $ dx*dx + dy*dy 
	where	dx = x2-x1
		dy = y2-y1

-- Snap system  point to x,y
snapTo :: Double -> Double -> System -> Int -> System
snapTo x y sys pnt = sys {baseShape=snapShapeTo x y (baseShape sys) pnt}
-- snap shape point tp x,y
snapShapeTo :: Double -> Double -> Shape -> Int -> Shape
snapShapeTo x y (Point (px,py)) _ = Point (x,y)
snapShapeTo x y (Line (x1,y1) (x2,y2)) sel 
	| sel==0 = Line (x,y) (x2,y2)
	| sel==1 = Line (x1,y1) (x,y)
	| otherwise = Line (x1,y1) (x2,y2)

snapShapeTo x y (Circle (h,k) r) sel 
	| sel==0 = Circle (x,y) r
	| sel==1 = Circle (h,k) dist
	| otherwise = Circle (h,k) r
	where	dist = sqrt $ dx * dx + dy * dy
		dx = x-h
		dy = y-k

-- returns true if System is within selectable distance of x,y
canSelectSys :: Double -> Double -> System -> Bool
canSelectSys x y sys = canSelectShape x y $ baseShape sys
-- returns true if Shape is within selectable distance of x,y
canSelectShape :: Double -> Double -> Shape -> Bool
canSelectShape x y (Point (px,py)) = x `closeTo` px && y `closeTo` py 
canSelectShape x y (Line (x1,y1) (x2,y2)) = (x `closeTo` x1 && y `closeTo` y1) || (x `closeTo` x2 && y `closeTo` y2) 
canSelectShape x y (Circle (h,k) r) = (x `closeTo` h && y `closeTo` k) || dist `closeTo` r
	where	dist = sqrt $ dx * dx + dy * dy
		dx = x-h
		dy = y-k
canSelectShape _ _ _ = False

selectPoint :: Double -> Double -> Shape -> Int
selectPoint x y (Point (px,py)) = 0
selectPoint x y (Line (x1,y1) (x2,y2))
	| x `closeTo` x1 && y `closeTo` y1 = 0
	| x `closeTo` x2 && y `closeTo` y2 = 1
	| otherwise = 0
selectPoint x y (Circle (h,k) r) 
	| (x `closeTo` h) && (y `closeTo` k) = 0
	| otherwise = 0
-- helper functions for selecting shapes
closeToGen :: (Num a, Ord a) => a -> a -> a -> Bool
closeToGen d a b = ( (b-d) < a && a < (b+d))
closeTo = (closeToGen 50.0)







------------------------ Register all callbacks
setWindowCallbacks :: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
setWindowCallbacks tc world win = do
	GLFW.setErrorCallback		   $ Just $ errorCallback
	GLFW.setWindowSizeCallback	win $ Just $ windowSizeCallback tc world
	GLFW.setWindowCloseCallback	win $ Just $ windowCloseCallback tc world
	GLFW.setWindowRefreshCallback	win $ Just $ windowRefreshCallback tc world
	GLFW.setMouseButtonCallback	win $ Just $ mouseButtonCallback tc world
	GLFW.setCursorPosCallback	win $ Just $ cursorPosCallback tc world
	GLFW.setKeyCallback		win $ Just $ keyCallback tc world
