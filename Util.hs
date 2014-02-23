module Util(
 resize
,distance
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
	
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
