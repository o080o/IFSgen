module Shapes (
Vector(..),
Shape(..),
Color(..),
drawShape,
isPoint,
isLine,
isCircle
) where

import qualified Graphics.Rendering.OpenGL as GL
import GHC.Float

type Vector = (Double,Double) 
data Shape = Point Vector | Line Vector Vector | Circle Vector Double deriving (Eq)
type Color = (Int,Int,Int)

pX = 3 -- 1/2 size of Point in pixels.

makeGLfloat :: Double -> GL.GLfloat
makeGLfloat = realToFrac

putGLVert :: Vector -> IO ()
putGLVert (x,y) = GL.vertex $ (GL.Vertex3 (makeGLfloat x) (makeGLfloat y) 0)
-- draws a shape with openGL. pattern matches for different shapes
drawShape :: Color -> Shape -> IO ()
drawShape (r,g,b) (Point (x,y)) = do
	GL.renderPrimitive GL.Quads $ do
		GL.color $ (GL.Color3 (realToFrac r) (realToFrac g) ((realToFrac b)::GL.GLfloat))
		putGLVert (x+pX,y+pX)
		putGLVert (x-pX,y+pX)
		putGLVert (x-pX,y-pX)
		putGLVert (x+pX,y-pX)

drawShape (r,g,b) (Line (x1,y1) (x2,y2)) = do
	GL.renderPrimitive GL.Lines $ do
		GL.color $ (GL.Color3 (realToFrac r) (realToFrac g) ((realToFrac b)::GL.GLfloat))
		GL.vertex $ (GL.Vertex3 (makeGLfloat x1) (makeGLfloat y1) 0)
		GL.vertex $ (GL.Vertex3 (makeGLfloat x2) (makeGLfloat y2) 0)

drawShape (red,grn,blu) (Circle (h,k) r) = do
	let	nsegment = 50::Int
		theta = 2*pi/(fromIntegral nsegment)
	GL.renderPrimitive GL.LineLoop $ do
		GL.color $ (GL.Color3 (realToFrac red) (realToFrac grn) ((realToFrac blu)::GL.GLfloat))
		mapM_ putGLVert $ circle (h,k) (sin theta) (cos theta) (r,0) nsegment

		


circle :: Vector -> Double -> Double -> Vector -> Int -> [Vector]
circle (h,k) _ _ (x,y) 0 = [(h+x,k+y)]
circle (h,k) s c (x,y) n = let	x' = c * x - s * y
				y' = s * x + c * y
	in (h+x',k+y'):(circle (h,k) s c (x',y') (n-1))

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
