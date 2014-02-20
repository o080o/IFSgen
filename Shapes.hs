module Shapes (
Vector(..),
Shape(..),
drawShape,
isPoint,
isLine,
isCircle
) where

import qualified Graphics.Rendering.OpenGL as GL
import GHC.Float

type Vector = (Double,Double)
data Shape = Point Vector | Line Vector Vector | Circle Vector Double

makeGLfloat :: Double -> GL.GLfloat
makeGLfloat = realToFrac
-- draws a shape with openGL. pattern matches for different shapes
drawShape :: Shape -> IO ()
drawShape (Point (x,y)) = do
	--GL.loadIdentity
	GL.renderPrimitive GL.Points $ do
		GL.vertex $ (GL.Vertex3 (makeGLfloat x) (makeGLfloat y) 0)

drawShape (Line (x1,y1) (x2,y2)) = do
	--GL.loadIdentity
	GL.renderPrimitive GL.Lines $ do
		GL.vertex $ (GL.Vertex3 (makeGLfloat x1) (makeGLfloat y1) 0)
		GL.vertex $ (GL.Vertex3 (makeGLfloat x2) (makeGLfloat y2) 0)

drawShape (Circle (h,k) r) = do
	--GL.loadIdentity
	GL.renderPrimitive GL.Points $ do
		GL.vertex $ (GL.Vertex3 (makeGLfloat h) (makeGLfloat k) 0)

-- is___ functions to test shapes (useful for filter)
isPoint :: Shape -> Bool
isPoint (Point (_,_)) = True
isPoint _ = False

isLine :: Shape -> Bool
isLine (Line (_,_) (_,_)) = True
isLine _ = False

isCircle :: Shape -> Bool
isCircle (Circle (_,_) _) = True
isCircle _ = False
