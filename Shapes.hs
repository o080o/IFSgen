module Shapes (
Vector(..),
Shape(..),
drawShape,
transform,
newPoint,
newLine,
newPoly,
newCircle,
asPoints,
asLines,
) where

import qualified Graphics.Rendering.OpenGL as GL
import GHC.Float
import Util

type Vector = (Double,Double) 
--data Shape = Point Vector | Line Vector Vector | Circle Vector Double deriving (Eq)
data Shape = Shape	{vertices::[Vector]
			,drawFunc::Shape -> IO()
			,rotation::Double
			,scale::Double
			,offset::Vector}

-- draw an arbitrary shape using its drawFunc
drawShape :: Shape -> IO ()
drawShape s = (drawFunc s) s

-- applies rotation, scale, and translation to a shape.
transform :: Double -> Double -> Vector -> Shape -> Shape
transform r s (dx,dy) shape = Shape	{vertices=points'
				,drawFunc=(drawFunc shape)
				,rotation = r + (rotation shape)
				,scale = s * (scale shape)
				,offset = (dx+dx',dy+dy')}

	where	sc (x,y) = (x*s, y*s)
		tr (x,y) = (x+dx,y+dy)
		ro (x,y) = ((x*cos r) - (y * sin r), (x * sin r) + (y * cos r))
		points = vertices shape
		points' = map (tr . sc . ro) points
		(dx',dy') = (sc . ro) (offset shape)


-- returns a new Point shape
newPoint :: Vector -> Shape
newPoint (x,y) = Shape	{vertices=[(x,y)]
			,drawFunc=(\s-> drawPoint (vertices s))
			,rotation = 0
			,scale = 1
			,offset = (x,y)}

-- returns a new Line shape
newLine :: Vector -> Vector -> Shape
newLine p1@(x1,y1) p2@(x2,y2) = Shape	{vertices=[p1,p2]
					,drawFunc=(\s-> drawLine (vertices s))
					,rotation = atan2 dy dx
					,scale = distance p1 p2
					,offset = p1}
	where	dx = x2-x1
		dy = y2-y1

-- returns a new Circle shape
newCircle :: Vector -> Vector -> Shape
newCircle p1 p2 = Shape	{vertices=[p1,p2]
			,drawFunc=(\s-> drawCircle (vertices s))
			,rotation = 0
			,scale = distance p1 p2
			,offset = p1}

newPoly :: [Vector] -> Shape
newPoly (p1@(x1,y1):p2@(x2,y2):p3:rest) = Shape {vertices=p1:p2:p3:rest
			,drawFunc=(\s-> drawPolygon (vertices s))
			,rotation=atan2 dy dx
			,scale=distance p1 p2
			,offset=p1}
	where	dx = x2-x1
		dy = y2-y1

-- make shapes out of control points!
asLines :: [Vector] -> [Shape]
asLines [] = []
asLines (_:[]) = []
asLines points = lines
	where (lines,lastp) = foldl (\(rest,lastp) p -> (newLine lastp p:rest,p)) ([],head points) (tail points)

asPoints :: [Vector] -> [Shape]
asPoints = map newPoint 
		

pX = 3 -- 1/2 size of Point in pixels.

makeGLfloat :: Double -> GL.GLfloat
makeGLfloat = realToFrac

putGLVert :: Vector -> IO ()
putGLVert (x,y) = GL.vertex $ (GL.Vertex3 (makeGLfloat x) (makeGLfloat y) 0)

-- draw a shape as a point
drawPoint :: [Vector] -> IO ()
drawPoint [] = return ()
drawPoint ((x,y):_) = do
	GL.renderPrimitive GL.Quads $ do
		putGLVert (x+pX,y+pX)
		putGLVert (x-pX,y+pX)
		putGLVert (x-pX,y-pX)
		putGLVert (x+pX,y-pX)

-- Draw a shape as a line
drawLine [] = return ()
drawLine (p:[]) = return ()
drawLine ((x1,y1):(x2,y2):_) = do
	GL.renderPrimitive GL.Lines $ do
		GL.vertex $ (GL.Vertex3 (makeGLfloat x1) (makeGLfloat y1) 0)
		GL.vertex $ (GL.Vertex3 (makeGLfloat x2) (makeGLfloat y2) 0)

drawPolygon points@(p1:p2:p3:rest) = do
	GL.renderPrimitive GL.Polygon $ do
		mapM_ putGLVert $ points
drawPolygon _ = return ()

-- Draw s shape as a circle
drawCircle [] = return ()
drawCircle (p:[]) = return ()
drawCircle ((h,k):p2:_) = do
	let	nsegment = 50::Int
		theta = 2*pi/(fromIntegral nsegment)
	GL.renderPrimitive GL.Polygon $ do
		mapM_ putGLVert $ circle (h,k) (sin theta) (cos theta) (r,0) nsegment
	where	r = (distance (h,k) p2)

-- draws a circle given certain paramters
circle :: Vector -> Double -> Double -> Vector -> Int -> [Vector]
circle (h,k) _ _ (x,y) 0 = [(h+x,k+y)]
circle (h,k) s c (x,y) n = let	x' = c * x - s * y
				y' = s * x + c * y
	in (h+x',k+y'):(circle (h,k) s c (x',y') (n-1))
