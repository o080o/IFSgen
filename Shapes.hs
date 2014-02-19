module Shapes (
Vector(..),
Shape(..),
isPoint,
isLine,
isCircle
) where

--data Vec = Vec (Number,Number)
type Vector = (Double,Double)
data Shape = Point Vector | Line Vector Vector | Circle Vector Double

isPoint :: Shape -> Bool
isPoint (Point (_,_)) = True
isPoint _ = False

isLine :: Shape -> Bool
isLine (Line (_,_) (_,_)) = True
isLine _ = False

isCircle :: Shape -> Bool
isCircle (Circle (_,_) _) = True
isCircle _ = False
