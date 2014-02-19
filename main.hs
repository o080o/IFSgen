import Graphics.UI.GLUT as G

import InputHandlers
import Shapes
import Functions



-- drawing handler
display :: [Shape] -> G.DisplayCallback
display shapes = do
	G.clear [G.ColorBuffer]
	G.swapBuffers

-- idle loop.
idleLoop :: Int -> IO ()
idleLoop a = return ()


-- Entry point.
main :: IO ()
main = do
	-- Initialize GLUT
	(progName, args) <- G.getArgsAndInitialize
	G.initialDisplayMode $= [G.DoubleBuffered]
	G.initialWindowSize $= G.Size 500 500
	G.createWindow "Fracture Fractal Generator"

	G.displayCallback $= display shapes
	G.keyboardMouseCallback $= Just (basicKBHandler)
	G.idleCallback $= Just (idleLoop 10)
	-- Start program!

	putStrLn "Entering GLUT main loop..."
	G.mainLoop
	where	shapes = []

