module Shape (
    Shape (),
    circle,
    ellipse,
    line,
    rect,
    square,
    polygon,
    (&),
    translate,
    fill,
    stroke,
    strokeWeight,
    scale,
    rotate,
    reflect,
) where

import Color
import Data.List (intercalate)
import Point

-- Data structure to represent shapes
data Shape
    = Shape {shape :: Shape, position :: Point, angle :: Float, fillColor :: Color, strokeColor :: Color, strokeThickness :: Float}
    | Ellipse Float Float -- horizontal radius, vertical radius
    | Polygon [Point] -- points
    | Group [Shape] -- shapes

-- Convert a shape to a JSON string
instance Show Shape where
    show (Shape shape position angle fill stroke strokeThickness) = "{ " ++ show shape ++ ", \"position\": " ++ show position ++ ", \"angle\": " ++ show angle ++ ", \"fill\": " ++ show fill ++ ", \"stroke\": " ++ show stroke ++ ", \"strokeWeight\": " ++ show strokeThickness ++ " }"
    show (Ellipse horizontalAxis verticalAxis) = "\"type\": \"ellipse\", \"horizontalAxis\": " ++ show horizontalAxis ++ ", \"verticalAxis\": " ++ show verticalAxis
    show (Polygon points) = "\"type\": \"polygon\", \"points\": " ++ show points
    show (Group shapes) = "[" ++ (intercalate ", " [show shape | shape <- shapes]) ++ "]"

-- Combine two shapes into a group
(&) :: Shape -> Shape -> Shape
(&) (Group (leftHead : leftTail)) (Group (rightHead : rightTail)) = Group (leftHead : leftTail ++ rightHead : rightTail)
(&) (Group (leftHead : leftTail)) rightShape = Group (leftHead : leftTail ++ [rightShape])
(&) leftShape (Group (rightHead : rightTail)) = Group (leftShape : rightHead : rightTail)
(&) leftShape rightShape = Group [leftShape, rightShape]

-- Functions to create shapes
circle :: Float -> Shape
circle radius = Shape (Ellipse radius radius) (Point 0 0) 0 Transparent Black 1

ellipse :: Float -> Float -> Shape
ellipse horizontalAxis verticalAxis = Shape (Ellipse horizontalAxis verticalAxis) (Point 0 0) 0 Transparent Black 1

line :: Float -> Shape
line length = Shape (Polygon [Point 0 0, Point length 0]) (Point 0 0) 0 Transparent Black 1

rect :: Float -> Float -> Shape
rect width height = Shape (Polygon [Point 0 0, Point width 0, Point width height, Point 0 height]) (Point 0 0) 0 Transparent Black 1

square :: Float -> Shape
square size = Shape (Polygon [Point 0 0, Point size 0, Point size size, Point 0 size]) (Point 0 0) 0 Transparent Black 1

polygon :: [Point] -> Shape
polygon points = Shape (Polygon points) (Point 0 0) 0 Transparent Black 1

-- Functions to manipulate shapes
translate :: Shape -> Point -> Shape
translate shape point = shape{position = point}

fill :: Shape -> Color -> Shape
fill shape color = shape{fillColor = color}

stroke :: Shape -> Color -> Shape
stroke shape color = shape{strokeColor = color}

strokeWeight :: Shape -> Float -> Shape
strokeWeight shape weight = shape{strokeThickness = weight}

scale :: Shape -> Float -> Shape
scale (Ellipse horizontalAxis verticalAxis) float = Ellipse (horizontalAxis * float) (verticalAxis * float)
scale (Polygon points) float = Polygon [Point (x * float) (y * float) | Point x y <- points]
scale (Group shapes) float = Group [scale shape float | shape <- shapes]

rotate :: Shape -> Float -> Shape
rotate shape angle = shape{angle = angle}

-- TODO: This is not actually a reflection
reflect :: Shape -> Float -> Shape
reflect shape angle = shape{angle = angle + 180}
