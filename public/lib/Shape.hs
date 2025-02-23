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
    bezier2,
    bezier3,
    arc,
    pie,
    (>>>),
    identityTransformation,
    translate,
    fill,
    stroke,
    strokeWeight,
    scale,
    rotate,
    -- reflect,
    noStroke,
    noFill,
    translateX,
    translateY,
) where

import Color (Color (Black, Transparent))
import Data.List (intercalate)
import Internal (removeFloat)
import Maths (Vector (..), (^+^))

----------------
---- Shapes ----
----------------

-- Data structures to represent shapes
data ShapeOptions = ShapeOptions
    { _position :: Vector
    , _angle :: Float
    , _fill :: Color
    , _stroke :: Color
    , _strokeWeight :: Float
    }
data Shape
    = Empty
    | Group [Shape]
    | Line
        { _length :: Float
        , _options :: ShapeOptions
        }
    | Ellipse
        { _horizontalAxis :: Float
        , _verticalAxis :: Float
        , _options :: ShapeOptions
        }
    | Rect
        { _width :: Float
        , _height :: Float
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
        , _verticalAxis :: Float
        , _startAngle :: Float
        , _endAngle :: Float
        , _open :: Bool
        , _options :: ShapeOptions
        }

-- Convert a shape to a JSON string
instance Show Shape where
    show :: Shape -> String
    show Empty = ""
    show (Group shapes) = "[" ++ (intercalate "," [show shape | shape <- shapes, not (isEmpty shape)]) ++ "]"
    show (Line length options) = "{\"t\":0,\"l\":" ++ removeFloat length ++ jsonOptionsNoFill options ++ "}"
    show (Ellipse horizontalAxis verticalAxis options) = "{\"t\":1,\"h\":" ++ removeFloat horizontalAxis ++ ",\"v\":" ++ removeFloat verticalAxis ++ jsonOptions options ++ "}"
    show (Rect width height options) = "{\"t\":2,\"w\":" ++ removeFloat width ++ ",\"h\":" ++ removeFloat height ++ jsonOptions options ++ "}"
    show (Polygon points options) = "{\"t\":3,\"v\":" ++ show points ++ jsonOptions options ++ "}"
    show (Curve points options) = "{\"t\":4,\"v\":" ++ show points ++ jsonOptions options ++ "}"
    show (Arc horizontalAxis verticalAxis startAngle endAngle open options) = "{\"t\":5,\"h\":" ++ removeFloat horizontalAxis ++ ",\"v\":" ++ removeFloat verticalAxis ++ ",\"b\":" ++ removeFloat startAngle ++ ",\"e\":" ++ removeFloat endAngle ++ ",\"o\":" ++ jsonBool open ++ jsonOptions options ++ "}"

-- Helper functions for converting shapes to JSON
isEmpty :: Shape -> Bool
isEmpty Empty = True
isEmpty _ = False

jsonPos :: Vector -> String
jsonPos (Vector x y)
    | x == 0 && y == 0 = ""
    | otherwise = ",\"p\":" ++ show (Vector x y)

jsonAng :: Float -> String
jsonAng a
    | a == defaultAngle = ""
    | otherwise = ",\"a\":" ++ removeFloat a

jsonFC :: Color -> String
jsonFC c
    | c == defaultFill = ""
    | otherwise = ",\"f\":" ++ show c

jsonS :: Color -> String
jsonS c
    | c == defaultStroke = ""
    | otherwise = ",\"s\":" ++ show c

jsonSW :: Float -> String
jsonSW w
    | w == defaultStrokeWeight = ""
    | otherwise = ",\"sw\":" ++ removeFloat w

jsonBool :: Bool -> String
jsonBool True = "1"
jsonBool False = "0"

jsonOptionsNoFill :: ShapeOptions -> String
jsonOptionsNoFill (ShapeOptions pos ang fc sc sw) = jsonPos pos ++ jsonAng ang ++ jsonS sc ++ jsonSW sw

jsonOptions :: ShapeOptions -> String
jsonOptions (ShapeOptions pos ang fc sc sw) = jsonPos pos ++ jsonAng ang ++ jsonFC fc ++ jsonS sc ++ jsonSW sw

-- Default arguments for shapes
defaultFill :: Color
defaultFill = Transparent

defaultStroke :: Color
defaultStroke = Black

defaultStrokeWeight :: Float
defaultStrokeWeight = 1

defaultPosition :: Vector
defaultPosition = Vector 0 0

defaultAngle :: Float
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
emptyShape :: Shape
emptyShape = Empty

-- Functions to create shapes
line :: Float -> Shape
line length = Line length defaultOptions

ellipse :: Float -> Float -> Shape
ellipse horizontalAxis verticalAxis = Ellipse horizontalAxis verticalAxis defaultOptions

circle :: Float -> Shape
circle radius = Ellipse radius radius defaultOptions

rect :: Float -> Float -> Shape
rect width height = Rect width height defaultOptions

square :: Float -> Shape
square size = Rect size size defaultOptions

polygon :: [Vector] -> Shape
polygon points = Polygon points defaultOptions

bezier2 :: Vector -> Vector -> Shape
bezier2 controlPoint endPoint = Curve [controlPoint, endPoint] defaultOptions

bezier3 :: Vector -> Vector -> Vector -> Shape
bezier3 controlPoint1 controlPoint2 endPoint = Curve [controlPoint1, controlPoint2, endPoint] defaultOptions

arc :: Float -> Float -> Float -> Float -> Shape
arc horizontalAxis verticalAxis startAngle endAngle = Arc horizontalAxis verticalAxis startAngle endAngle True defaultOptions

pie :: Float -> Float -> Float -> Float -> Shape
pie horizontalAxis verticalAxis startAngle endAngle = Arc horizontalAxis verticalAxis startAngle endAngle False defaultOptions

-------------------------
---- Transformations ----
-------------------------

-- Chain transformations
(>>>) :: Shape -> (Shape -> Shape) -> Shape
(>>>) shape transform = transform shape

-- Identity transformation
identityTransformation :: Shape -> Shape
identityTransformation shape = shape

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

rotate :: Float -> Shape -> Shape
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

-- TODO: This isn't actually a reflection
reflect :: Float -> Shape -> Shape
reflect _ Empty = Empty
reflect angle (Group shapes) = group shapes (reflect angle)
reflect angle shape = shape{_options = (_options shape){_angle = angle + 180}}

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
