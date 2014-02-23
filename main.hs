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

--defaultSys =	[System {baseShape=bs1,replaceShapes=koch,sysColor=(0,0,0),fillType=Fill}
		--,System {baseShape=bs3,replaceShapes=koch2,sysColor=(0,0,0),fillType=Fill}
		--,System {baseShape=bs2,replaceShapes=koch3,sysColor=(0,0,0),fillType=Fill}]
defaultSys :: IO [System]
defaultSys = do
	sys1 <- newSystem bs1 koch
	sys2 <- newSystem bs2 koch2
	sys3 <- newSystem bs3 koch3
	return [sys1,sys2,sys3]
	where	bs1=Line (50,50) (150,150)
		bs2=Line (100,100) (200,200)
		bs3 = Line (200,50) (50,200)

-- drawing handler. draws shapes.
drawShapes :: GLFW.Window -> [Shape] -> Shape -> IO()
drawShapes win shapes selShape = do
	GL.loadIdentity
	mapM_ (drawShape (255,255,255)) shapes 
	drawShape (255,0,0) selShape 
	GL.flush

drawSys :: System -> IO ()
drawSys sys = do
	let	c = sysColor sys
		s = baseShape sys
	drawShape c s
	return ()
	
drawSystems :: GLFW.Window -> [System] -> IO ()
drawSystems win sys = do
	GL.loadIdentity
	mapM_ (drawSys) sys
	GL.flush

-- main program loop. Nothing interesting. InputHandlers is more interesting.
mainLoop :: STM.TChan InputMsg -> WorldState -> GLFW.Window -> IO ()
mainLoop ch worldState win = do
	GLFW.waitEvents
	(sys, test) <- STM.atomically $ do
		world <- STM.readTVar worldState
		return $ ((selectedSys world):(systems world), selectedPoint world)
		
	--draw win shapes selShape
	
	GL.clear [GL.ColorBuffer, GL.DepthBuffer]
	drawSystems win sys
	GLFW.swapBuffers win


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
			sys <- defaultSys
			worldState <- newWorld sys
			
			GLFW.makeContextCurrent window
			setWindowCallbacks ch worldState win
			GLFW.swapInterval 1
			GL.clearColor $= GL.Color4 1 1 1 1
			putStrLn "Entering mainLoop..."
			mainLoop ch worldState win 
			GLFW.destroyWindow win
		Nothing -> return ()
	GLFW.terminate
	where n = 0
	


