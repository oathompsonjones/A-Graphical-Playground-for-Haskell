module Shape (
    Shape (),
    (&),
    emptyShape,
    circle,
    ellipse,
    line,
    rect,
    square,
    polygon,
    (>>>),
    identityTransformation,
    translate,
    fill,
    stroke,
    strokeWeight,
    scale,
    rotate,
    reflect,
) where

import Color (Color (Black, Transparent))
import Data.List (intercalate)
import Maths (Vector (..))

----------------
---- Shapes ----
----------------

-- Data structure to represent shapes
data Shape
    = Shape {shape :: Shape, position :: Vector, angle :: Float, fillColor :: Color, strokeColor :: Color, strokeThickness :: Float}
    | Ellipse Float Float -- horizontal radius, vertical radius
    | Polygon [Vector] -- points
    | Group [Shape] -- shapes
    | Empty

-- Convert a shape to a JSON string
instance Show Shape where
    show :: Shape -> String
    show (Shape shape position angle fill stroke strokeThickness) = "{ " ++ show shape ++ ", \"position\": " ++ show position ++ ", \"angle\": " ++ show angle ++ ", \"fill\": " ++ show fill ++ ", \"stroke\": " ++ show stroke ++ ", \"strokeWeight\": " ++ show strokeThickness ++ " }"
    show (Ellipse horizontalAxis verticalAxis) = "\"type\": \"ellipse\", \"horizontalAxis\": " ++ show horizontalAxis ++ ", \"verticalAxis\": " ++ show verticalAxis
    show (Polygon points) = "\"type\": \"polygon\", \"points\": " ++ show points
    show (Group shapes) = "[" ++ (intercalate ", " [show shape | shape <- shapes]) ++ "]"

-- Combine two shapes into a group
(&) :: Shape -> Shape -> Shape
(&) Empty right = right
(&) left Empty = left
(&) (Group left) (Group right) = Group (left ++ right)
(&) (Group left) right = Group (left ++ [right])
(&) left (Group right) = Group (left : right)
(&) left right = Group [left, right]

-- Identity shape
emptyShape :: Shape
emptyShape = Shape Empty (Vector 0 0) 0 Transparent Black 1

-- Functions to create shapes
circle :: Float -> Shape
circle radius = Shape (Ellipse radius radius) (Vector 0 0) 0 Transparent Black 1

ellipse :: Float -> Float -> Shape
ellipse horizontalAxis verticalAxis = Shape (Ellipse horizontalAxis verticalAxis) (Vector 0 0) 0 Transparent Black 1

line :: Float -> Shape
line length = Shape (Polygon [Vector 0 0, Vector length 0]) (Vector 0 0) 0 Transparent Black 1

rect :: Float -> Float -> Shape
rect width height = Shape (Polygon [Vector 0 0, Vector width 0, Vector width height, Vector 0 height]) (Vector 0 0) 0 Transparent Black 1

square :: Float -> Shape
square size = Shape (Polygon [Vector 0 0, Vector size 0, Vector size size, Vector 0 size]) (Vector 0 0) 0 Transparent Black 1

polygon :: [Vector] -> Shape
polygon points = Shape (Polygon points) (Vector 0 0) 0 Transparent Black 1

-------------------------
---- Transformations ----
-------------------------

-- Chain transformations
(>>>) :: Shape -> (Shape -> Shape) -> Shape
(>>>) shape function = function shape

-- Identity transformation
identityTransformation :: Shape -> Shape
identityTransformation shape = shape

-- Functions to manipulate shapes
fill :: Color -> Shape -> Shape
fill color shape = shape{fillColor = color}

stroke :: Color -> Shape -> Shape
stroke color shape = shape{strokeColor = color}

strokeWeight :: Float -> Shape -> Shape
strokeWeight weight shape = shape{strokeThickness = weight}

translate :: Vector -> Shape -> Shape
translate vector shape = shape{position = Vector (x vector + x (position shape)) (y vector + y (position shape))}

rotate :: Float -> Shape -> Shape
rotate angle shape = shape{angle = angle}

-- TODO: This doesn't work
scale :: Float -> Shape -> Shape
scale scaleFactor (Ellipse horizontalAxis verticalAxis) = Ellipse (horizontalAxis * scaleFactor) (verticalAxis * scaleFactor)
scale scaleFactor (Polygon points) = Polygon [Vector (x * scaleFactor) (y * scaleFactor) | Vector x y <- points]
scale scaleFactor (Group shapes) = Group [scale scaleFactor shape | shape <- shapes]
scale scaleFactor shape' = scale scaleFactor (shape shape')

-- TODO: This isn't actually a reflection
reflect :: Float -> Shape -> Shape
reflect angle shape = shape{angle = angle + 180}
