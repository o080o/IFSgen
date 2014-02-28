import Control.Monad
import Control.Concurrent
import Control.Exception
import qualified Control.Concurrent.STM as STM
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import Util
import InputHandlers
import Shapes
import WorldData
import System
import Iterators

--defaultSys =	[System {baseShape=bs1,replaceShapes=koch,sysColor=(0,0,0),fillType=Fill}
		--,System {baseShape=bs3,replaceShapes=koch2,sysColor=(0,0,0),fillType=Fill}
		--,System {baseShape=bs2,replaceShapes=koch3,sysColor=(0,0,0),fillType=Fill}]
defaultSys :: IO [System]
defaultSys = do
	sys1 <- newKoch (175,175) (200,150)
	sys2 <- newIFS bs2 koch2
	sys3 <- newIFS bs3 koch3
	return [sys1,sys2,sys3]
	where	bs1=newLine (50,50) (150,150)
		bs2=newLine (100,100) (200,200)
		bs3 =newLine (200,50) (50,200)

-- drawing handler. draws shapes.
--drawShapes :: GLFW.Window -> [Shape] -> Shape -> IO()
--drawShapes win shapes selShape = do
--	GL.loadIdentity
--	mapM_ (drawShape (255,255,255)) shapes 
--	drawShape (255,0,0) selShape 
--	GL.flush
--
makeGLfloat :: Double -> GL.GLfloat
makeGLfloat = realToFrac
setColor (r,g,b) = GL.renderPrimitive GL.Points $ do
	GL.color $ (GL.Color3 (makeGLfloat r) (makeGLfloat g) (makeGLfloat b))
	GL.color $ (GL.Color3 (makeGLfloat r) (makeGLfloat g) (makeGLfloat b))

drawSys :: System -> IO ()
drawSys sys = do
	let	base = (baseShapes sys) (controlPoints sys)
	shapes <- STM.atomically $ STM.readTVar $ drawShapes sys
	setColor (0,0,0)
	mapM_ (drawShape) shapes
	setColor (1,0,0)
	mapM_ (drawShape) base
	return ()
	
-- drawing handler. draws systems.
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
	-- close program when window closes. (NYI)
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
			-- TChannel for window messages (NYI)
			winch <- STM.newTChanIO :: IO (STM.TChan InputMsg)
			-- TChannel for System solving queue
			ch <- STM.newTChanIO :: IO (STM.TChan System)
			sys <- defaultSys
			worldState <- newWorld sys
			
			GLFW.makeContextCurrent window
			setWindowCallbacks winch ch worldState win
			GLFW.swapInterval 1
			GL.clearColor $= GL.Color4 1 1 1 1
			GL.lineWidth $= 3
			GL.lineSmooth $= GL.Enabled
			resize 500 500
			putStrLn "Entering mainLoop..."
			forkIO $ watchSystems ch []
			mainLoop winch worldState win 
			GLFW.destroyWindow win
		Nothing -> return ()
	GLFW.terminate
	where n = 0
	


