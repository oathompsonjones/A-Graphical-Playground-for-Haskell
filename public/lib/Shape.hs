module Shape where

import Color (Color (Black, Transparent))
import Data.List (intercalate)
import Maths (Length, Radians, Vector (..), (^+^))

----------------
---- Shapes ----
----------------

-- Data structures to represent shapes
data Connection = Open | Chord | Pie
data ShapeOptions = ShapeOptions
    { _position :: Vector
    , _angle :: Radians
    , _fill :: Color
    , _stroke :: Color
    , _strokeWeight :: Float
    }
data Shape
    = Empty
    | Group [Shape]
    | Line
        { _length :: Length
        , _options :: ShapeOptions
        }
    | Ellipse
        { _horizontalAxis :: Length
        , _verticalAxis :: Length
        , _options :: ShapeOptions
        }
    | Rect
        { _width :: Length
        , _height :: Length
        , _options :: ShapeOptions
        }
    | Polygon
        { _points :: [Vector]
        , _options :: ShapeOptions
        }
    | Curve
        { _points :: [Vector]
        , _options :: ShapeOptions
        }
    | Arc
        { _horizontalAxis :: Float
        , _verticalAxis :: Length
        , _startAngle :: Radians
        , _endAngle :: Radians
        , _connect :: Connection
        , _options :: ShapeOptions
        }

-- Default arguments for shapes
defaultFill :: Color
defaultFill = Transparent

defaultStroke :: Color
defaultStroke = Black

defaultStrokeWeight :: Float
defaultStrokeWeight = 1

defaultPosition :: Vector
defaultPosition = Vector 0 0

defaultAngle :: Radians
defaultAngle = 0

defaultOptions :: ShapeOptions
defaultOptions = ShapeOptions defaultPosition defaultAngle defaultFill defaultStroke defaultStrokeWeight

-- Combine two shapes into a group
(&) :: Shape -> Shape -> Shape
(&) Empty right = right
(&) left Empty = left
(&) (Group left) (Group right) = Group (left ++ right)
(&) (Group left) right = Group (left ++ [right])
(&) left (Group right) = Group (left : right)
(&) left right = Group [left, right]

-- Identity shape
empty :: Shape
empty = Empty

-- Functions to create shapes
line :: Length -> Shape
line length = Line length defaultOptions

ellipse :: Length -> Length -> Shape
ellipse horizontalAxis verticalAxis = Ellipse horizontalAxis verticalAxis defaultOptions

circle :: Length -> Shape
circle radius = Ellipse radius radius defaultOptions

rect :: Length -> Length -> Shape
rect width height = Rect width height defaultOptions

square :: Length -> Shape
square size = Rect size size defaultOptions

polygon :: [Vector] -> Shape
polygon points = Polygon points defaultOptions

bezier2 :: Vector -> Vector -> Shape
bezier2 controlPoint endPoint = Curve [controlPoint, endPoint] defaultOptions

bezier3 :: Vector -> Vector -> Vector -> Shape
bezier3 controlPoint1 controlPoint2 endPoint = Curve [controlPoint1, controlPoint2, endPoint] defaultOptions

arc :: Length -> Length -> Radians -> Radians -> Shape
arc horizontalAxis verticalAxis startAngle endAngle = Arc horizontalAxis verticalAxis startAngle endAngle Open defaultOptions

segment :: Length -> Length -> Radians -> Radians -> Shape
segment horizontalAxis verticalAxis startAngle endAngle = Arc horizontalAxis verticalAxis startAngle endAngle Chord defaultOptions

pie :: Length -> Length -> Radians -> Radians -> Shape
pie horizontalAxis verticalAxis startAngle endAngle = Arc horizontalAxis verticalAxis startAngle endAngle Pie defaultOptions

-------------------------
---- Transformations ----
-------------------------

-- Chain transformations
(>>>) :: Shape -> (Shape -> Shape) -> Shape
(>>>) shape transform = transform shape

-- Apply a transformation to a group of shapes
group :: [Shape] -> (Shape -> Shape) -> Shape
group shapes transform = Group [transform shape | shape <- shapes]

-- Functions to manipulate shapes
fill :: Color -> Shape -> Shape
fill _ Empty = Empty
fill color (Group shapes) = group shapes (fill color)
fill color shape = shape{_options = (_options shape){_fill = color}}

stroke :: Color -> Shape -> Shape
stroke _ Empty = Empty
stroke color (Group shapes) = group shapes (stroke color)
stroke color shape = shape{_options = (_options shape){_stroke = color}}

strokeWeight :: Float -> Shape -> Shape
strokeWeight _ Empty = Empty
strokeWeight weight (Group shapes) = group shapes (strokeWeight weight)
strokeWeight weight shape = shape{_options = (_options shape){_strokeWeight = weight}}

translate :: Vector -> Shape -> Shape
translate _ Empty = Empty
translate vector (Group shapes) = group shapes (translate vector)
translate vector shape = shape{_options = (_options shape){_position = _position (_options shape) ^+^ vector}}

rotate :: Radians -> Shape -> Shape
rotate _ Empty = Empty
rotate angle (Group shapes) = group shapes (rotate angle)
rotate angle shape = shape{_options = (_options shape){_angle = angle}}

scale :: Float -> Shape -> Shape
scale _ Empty = Empty
scale scaleFactor (Group shapes) = group shapes (scale scaleFactor)
scale scaleFactor (Line length options) = Line (length * scaleFactor) options
scale scaleFactor (Ellipse horizontalAxis verticalAxis options) = Ellipse (horizontalAxis * scaleFactor) (verticalAxis * scaleFactor) options
scale scaleFactor (Rect width height options) = Rect (width * scaleFactor) (height * scaleFactor) options
scale scaleFactor (Polygon points options) = Polygon [Vector (x * scaleFactor) (y * scaleFactor) | Vector x y <- points] options
scale scaleFactor (Curve points options) = Curve [Vector (x * scaleFactor) (y * scaleFactor) | Vector x y <- points] options
scale scaleFactor (Arc horizontalAxis verticalAxis startAngle endAngle open options) = Arc (horizontalAxis * scaleFactor) (verticalAxis * scaleFactor) startAngle endAngle open options

-- Shorthand transformations
noStroke :: Shape -> Shape
noStroke = stroke Transparent

noFill :: Shape -> Shape
noFill = fill Transparent

translateX :: Float -> Shape -> Shape
translateX x = translate (Vector x 0)

translateY :: Float -> Shape -> Shape
translateY y = translate (Vector 0 y)

-- Set operator precedence
infixr 8 &
infixl 9 >>>
