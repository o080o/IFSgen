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
mouseButtonCallback tc worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = STM.atomically $ do
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
mouseButtonCallback tc worldState win GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = STM.atomically $ do
	world <- STM.readTVar worldState
	STM.writeTVar worldState $ world	{mouseDown=False
						,mouseDragging=False
						,selectedPoint=Nothing}
-- catch-all mouseButton pattern
mouseButtonCallback tc worldState win _ _ _ = return ()

-- mouse move
cursorPosCallback tc worldState win x y = STM.atomically $ do
	world <- STM.readTVar worldState
	-- test for mosue drag conditions
	if mouseDown world && (mouseDragging world || distance (firstPos world) (x,y)>dragThresh) then
		let	sel = selectedSys world
			pnt = selectedPoint world
		 	sys = transformSys sel pnt
		in STM.writeTVar worldState $ world	{mouseDragging=True
							,selectedSys=sys
							,lastPos=(x,y)}
	else STM.writeTVar worldState world {lastPos=(x,y)}
	where	transformSys sys Nothing = sys
		transformSys sys (Just pnt) = snapTo x y sys pnt
-- keyboard related callbacks
keyCallback tc worldState win key sc keyState mod = putStrLn $ "key" ++ (show key) ++ (show sc) ++ (show keyState)
	
---------- helper functions-------------	
--
-- pick a system to select out of the world. 
pickSys :: Double -> Double -> World -> ([System],System,Maybe Int)
pickSys x y world =	let	oldsel = selectedSys world
				oldsys = systems world
				(newsel,pnt) = pick selectable
				newsys = filter (newsel /=) (oldsel:oldsys)
			in (newsys,newsel,pnt)
	where	selectable = filter (canSelectSys x y)  ((selectedSys world):(systems world)) -- returns a list of possible selectable systems
		pick [] = (selectedSys world,Nothing)
		pick (s:_) = (s,Just $ selectPoint x y (baseShape s))

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
	| sel==1 = Circle (h,k) $ distance (x,y) (h,k)
	| otherwise = Circle (h,k) r
snapShapeTo x y s _ = s -- unknown shape constructor

-- returns true if System is within selectable distance of x,y
canSelectSys :: Double -> Double -> System -> Bool
canSelectSys x y sys = canSelectShape x y $ baseShape sys
-- returns true if Shape is within selectable distance of x,y
canSelectShape :: Double -> Double -> Shape -> Bool
canSelectShape x y (Point (px,py)) = x `closeTo` px && y `closeTo` py 
canSelectShape x y (Line (x1,y1) (x2,y2)) = (x `closeTo` x1 && y `closeTo` y1) || (x `closeTo` x2 && y `closeTo` y2) 
canSelectShape x y (Circle (h,k) r) = (x `closeTo` h && y `closeTo` k) || (distance (x,y) (h,k)) `closeTo` r
canSelectShape x y _ = False --unknown shape constructor

selectPoint :: Double -> Double -> Shape -> Int
selectPoint x y (Point (px,py)) = 0
selectPoint x y (Line (x1,y1) (x2,y2))
	| x `closeTo` x1 && y `closeTo` y1 = 0
	| x `closeTo` x2 && y `closeTo` y2 = 1
	| otherwise = 0
selectPoint x y (Circle (h,k) r) 
	| (x `closeTo` h) && (y `closeTo` k) = 0
	| otherwise = 0
selectPoint x y _ = 0 --unknown shape constructor
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
