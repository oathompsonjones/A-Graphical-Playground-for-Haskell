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
    noStroke,
    noFill,
    translateX,
    translateY,
) where

import Color (Color (Black, Transparent))
import Data.List (intercalate)
import Maths (Vector (..), (^+^))

----------------
---- Shapes ----
----------------

-- Data structure to represent shapes
data Shape
    = Ellipse {hAx :: Float, vAx :: Float, pos :: Vector, ang :: Float, fc :: Color, sc :: Color, sw :: Float}
    | Polygon {pts :: [Vector], pos :: Vector, ang :: Float, fc :: Color, sc :: Color, sw :: Float}
    | Group [Shape]
    | Empty

-- Convert a shape to a JSON string
instance Show Shape where
    show :: Shape -> String
    show (Ellipse hAx vAx pos ang fc sc sw) =
        "{ "
            ++ "\"type\": \"ellipse\", \"horizontalAxis\": "
            ++ show hAx
            ++ ", \"verticalAxis\": "
            ++ show vAx
            ++ ", \"position\": "
            ++ show pos
            ++ ", \"angle\": "
            ++ show ang
            ++ ", \"fill\": "
            ++ show fc
            ++ ", \"stroke\": "
            ++ show sc
            ++ ", \"strokeWeight\": "
            ++ show sw
            ++ " }"
    show (Polygon pts pos ang fc sc sw) =
        "{ "
            ++ "\"type\": \"polygon\", \"points\": "
            ++ show pts
            ++ ", \"position\": "
            ++ show pos
            ++ ", \"angle\": "
            ++ show ang
            ++ ", \"fill\": "
            ++ show fc
            ++ ", \"stroke\": "
            ++ show sc
            ++ ", \"strokeWeight\": "
            ++ show sw
            ++ " }"
    show (Group ss) = "[" ++ (intercalate ", " [show s | s <- ss]) ++ "]"

-- Default arguments for shapes
defFC :: Color
defFC = Transparent

defSC :: Color
defSC = Black

defSW :: Float
defSW = 1

defPos :: Vector
defPos = Vector 0 0

defAng :: Float
defAng = 0

-- Combine two shapes into a group
(&) :: Shape -> Shape -> Shape
(&) Empty r = r
(&) l Empty = l
(&) (Group l) (Group r) = Group (l ++ r)
(&) (Group l) r = Group (l ++ [r])
(&) l (Group r) = Group (l : r)
(&) l r = Group [l, r]

-- Identity shape
emptyShape :: Shape
emptyShape = Empty

-- Functions to create shapes
circle :: Float -> Shape
circle r = Ellipse r r defPos defAng defFC defSC defSW

ellipse :: Float -> Float -> Shape
ellipse hAx vAx = Ellipse hAx vAx defPos defAng defFC defSC defSW

line :: Float -> Shape
line l = Polygon [Vector 0 0, Vector l 0] defPos defAng defFC defSC defSW

rect :: Float -> Float -> Shape
rect w h = Polygon [Vector 0 0, Vector w 0, Vector w h, Vector 0 h] defPos defAng defFC defSC defSW

square :: Float -> Shape
square s = Polygon [Vector 0 0, Vector s 0, Vector s s, Vector 0 s] defPos defAng defFC defSC defSW

polygon :: [Vector] -> Shape
polygon pts = Polygon pts defPos defAng defFC defSC defSW

-------------------------
---- Transformations ----
-------------------------

-- Chain transformations
(>>>) :: Shape -> (Shape -> Shape) -> Shape
(>>>) s f = f s

-- Identity transformation
identityTransformation :: Shape -> Shape
identityTransformation s = s

-- Functions to manipulate shapes
fill :: Color -> Shape -> Shape
fill c Empty = Empty
fill c (Group ss) = Group [fill c s | s <- ss]
fill c s = s{fc = c}

stroke :: Color -> Shape -> Shape
stroke c Empty = Empty
stroke c (Group ss) = Group [stroke c s | s <- ss]
stroke c s = s{sc = c}

strokeWeight :: Float -> Shape -> Shape
strokeWeight w Empty = Empty
strokeWeight w (Group ss) = Group [strokeWeight w s | s <- ss]
strokeWeight w s = s{sw = w}

translate :: Vector -> Shape -> Shape
translate v Empty = Empty
translate v (Group ss) = Group [translate v s | s <- ss]
translate v s = s{pos = pos s ^+^ v}

rotate :: Float -> Shape -> Shape
rotate a Empty = Empty
rotate a (Group ss) = Group [rotate a s | s <- ss]
rotate a s = s{ang = a}

-- TODO: This doesn't work
scale :: Float -> Shape -> Shape
scale f Empty = Empty
scale f (Group ss) = Group [scale f s | s <- ss]
scale f (Ellipse hAx vAx pos ang fc sc sw) = Ellipse (hAx * f) (vAx * f) pos ang fc sc sw
scale f (Polygon pts pos ang fc sc sw) = Polygon [Vector (x * f) (y * f) | Vector x y <- pts] pos ang fc sc sw

-- TODO: This isn't actually a reflection
reflect :: Float -> Shape -> Shape
reflect a Empty = Empty
reflect a (Group ss) = Group [reflect a s | s <- ss]
reflect a s = s{ang = a + 180}

-- Shorthand transformations
noStroke :: Shape -> Shape
noStroke = stroke Transparent

noFill :: Shape -> Shape
noFill = fill Transparent

translateX :: Float -> Shape -> Shape
translateX x = translate (Vector x 0)

translateY :: Float -> Shape -> Shape
translateY y = translate (Vector 0 y)
