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

-- Delete selected system on DELETE key
keyCallback tc worldState _ GLFW.Key'Delete _ GLFW.KeyState'Pressed _ = STM.atomically $ do
	world <- STM.readTVar worldState
	let	newsys = systems world
	if length newsys > 1 then
		STM.writeTVar worldState $ world	{systems=tail newsys,selectedSys=head newsys}
	else
		return ()
-- Add a new system on SPACE key
keyCallback tc worldState _ GLFW.Key'Space _ GLFW.KeyState'Pressed _ = do
	sys <- newSystem (Point (0,0)) koch -- make a new system while in IO
	STM.atomically $ do
		world <- STM.readTVar worldState
		let	(x,y) = lastPos world
			sys' = sys { baseShape=Point (x,y) }
			sel = selectedSys world
		STM.writeTVar worldState $ world	{systems=sel:(systems world)
							,selectedSys=sys'}

-- increase iterations on +/= key
keyCallback tc worldState _ GLFW.Key'Equal _ GLFW.KeyState'Pressed _ = STM.atomically $ do
	world <- STM.readTVar worldState
	let	sel = selectedSys world
		sel' = sel { iter=(iter sel) + 1 }
	STM.writeTVar worldState $ world	{selectedSys=sel'}
-- decrease iterations on -/_ key
keyCallback tc worldState _ GLFW.Key'Minus _ GLFW.KeyState'Pressed _ = STM.atomically $ do
	world <- STM.readTVar worldState
	let	sel = selectedSys world
		sel' = sel { iter=(iter sel) - 1 }
	STM.writeTVar worldState $ world	{selectedSys=sel'}
			
-- Change baseShape type on P key
keyCallback tc worldState _ GLFW.Key'P _ GLFW.KeyState'Pressed _ = changeBaseShape worldState (changeTo)
	where	changeTo (Point p) = Point p
		changeTo (Line p p2) = Point $ midpoint p p2
		changeTo (Circle p _) = Point p
-- Change baseShape type on L key
keyCallback tc worldState _ GLFW.Key'L _ GLFW.KeyState'Pressed _ = changeBaseShape worldState (changeTo)
	where	changeTo (Point (x,y)) = Line (x-50,y) (x+50,y)
		changeTo (Line p p2) = Line p p2
		changeTo (Circle (h,k) r) = Line (h-r,k) (h+r,k)
-- Change baseShape type on L key
keyCallback tc worldState _ GLFW.Key'C _ GLFW.KeyState'Pressed _ = changeBaseShape worldState (changeTo)
	where	changeTo (Point p) = Circle p 50
		changeTo (Line p p2) = Circle (midpoint p p2) ((distance p p2)/2)
		changeTo (Circle p r) = Circle p r

-- catch-all keyboard callback
keyCallback tc worldState win key sc keyState mod = putStrLn $ "key" ++ (show key) ++ " " ++ (show sc) ++ " " ++ (show keyState) ++ " " ++ (show mod)
	
-- body of keyhandlers 'T' 'L' and 'C'. changed one type of shape into another.
changeBaseShape :: WorldState -> (Shape -> Shape) -> IO ()
changeBaseShape worldState f = STM.atomically $ do
	world <- STM.readTVar worldState
	let	sel=selectedSys world
		bs=baseShape sel
		newsel=sel {baseShape=f bs}
	STM.writeTVar worldState $ world	{selectedSys=newsel}
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
	| otherwise = 1
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
