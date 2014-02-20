import Control.Monad
import Control.Exception
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import InputHandlers
import Shapes
import Functions

-- drawing handler
draw :: GLFW.Window -> [Shape] -> IO()
draw win shapes = do
	GL.clear [GL.ColorBuffer, GL.DepthBuffer]

	GL.loadIdentity
	mapM_ drawShape shapes
	GL.flush
	GLFW.swapBuffers win

-- resize callback
resize :: Int -> Int -> IO ()
resize w h = do
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
	GL.matrixMode $= GL.Projection
	GL.loadIdentity
	GL.perspective 45 (fromIntegral w / fromIntegral h) 1 100
	GL.matrixMode $= GL.Modelview 0

-- some basic shapes.
shapes1 =  		[Line (-1,0) (1,0)
			,Line (1,0) (1,1)
			,Point (0.5,0.5)
			,Point (0.25,0.25)]
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

-- main program loop.
mainLoop :: GLFW.Window -> Double -> Double -> Vector -> IO ()
mainLoop win r s (tx,ty) = do
	putStrLn "Loop:"
	--draw win shapes1
	draw win $ concat [iterateSystem baseline2 koch 2,iterateSystem baseline3 koch2 4, iterateSystem baseline koch3 3,[baseline2,baseline3]]
	GLFW.pollEvents
	putStrLn "Done."
	mainLoop win (r-0.01) (s-0.01) (tx-0.01,ty-0.01)
	where 	baseline = Line (-0.9,0.1) (0.9,0.1)
		baseline2 = Line (0.5,-0.9) (0.5,0.9)
		baseline3 = Line (0.5,0.9) (0.5,-0.9)
		
	

-- Entry point.
main :: IO ()
main = do
	-- initialize GLFW. crashes with pattern match error on failure
	True <- GLFW.init
	window <- GLFW.createWindow 500 500 "Title" Nothing Nothing
	case window of
		(Just win) -> do
			GLFW.makeContextCurrent window
			setWindowCallbacks win
			GLFW.swapInterval 1
			putStrLn "Entering mainLoop..."
			mainLoop win 0 2 (0,0)
			GLFW.destroyWindow win
		Nothing -> return ()
	GLFW.terminate
	where n = 0
	


