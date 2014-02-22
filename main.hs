import Control.Monad
import Control.Exception
import qualified Control.Concurrent.STM as STM
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import InputHandlers
import Shapes
import Functions
import WorldData

defaultSys =	[System {baseShape=bs1,replaceShapes=koch,sysColor=(0,0,0),fillType=Fill}
		,System {baseShape=bs3,replaceShapes=koch2,sysColor=(0,0,0),fillType=Fill}
		,System {baseShape=bs2,replaceShapes=koch3,sysColor=(0,0,0),fillType=Fill}]
	where	bs1=Line (50,50) (150,150)
		bs2=Line (0,0) (200,200)
		bs3 = Line (200,0) (0,200)

-- drawing handler. draws shapes.
draw :: GLFW.Window -> [Shape] -> Shape -> IO()
draw win shapes selShape = do
	GL.clear [GL.ColorBuffer, GL.DepthBuffer]

	GL.loadIdentity
	mapM_ (drawShape (255,255,255)) shapes 
	drawShape (255,0,0) selShape 
	GL.flush
	GLFW.swapBuffers win

-- main program loop. Nothing interesting. InputHandlers is more interesting.
mainLoop :: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
mainLoop ch worldState win = do
	GLFW.waitEvents
	(shapes,selShape, test) <- STM.atomically $ do
		world <- STM.readTVar worldState
		return $ (map (baseShape) (systems world), baseShape (selectedSys world), selectedPoint world)
		
	draw win shapes selShape
	putStrLn $ "Select:" ++ (show test)
	-- close program when window closes.
	isOpen <- return True
	when isOpen $ mainLoop ch worldState win

-- Entry point.
main :: IO ()
main = do
	-- initialize GLFW. crashes with pattern match error on failure
	True <- GLFW.init
	window <- GLFW.createWindow 500 500 "Title" Nothing Nothing
	case window of
		(Just win) -> do
			ch <- STM.newTChanIO :: IO (STM.TChan InputMsg)
			worldState <- newWorld defaultSys
			
			GLFW.makeContextCurrent window
			setWindowCallbacks ch worldState win
			GLFW.swapInterval 1
			putStrLn "Entering mainLoop..."
			mainLoop ch worldState win 
			GLFW.destroyWindow win
		Nothing -> return ()
	GLFW.terminate
	where n = 0
	


