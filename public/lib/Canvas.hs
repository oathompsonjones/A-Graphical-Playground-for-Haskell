module Canvas where

import Color (Color (Transparent))
import Maths (Length, Vector (Vector), arg, mag)
import Shape (
    Shape (Curve, Empty, Group, Line, Rect, _options),
    ShapeOptions (_angle, _position),
    group,
 )

-- Data structure to represent a canvas
data Canvas = Canvas
    { _width :: Length
    , _height :: Length
    , _fps :: Int
    , _backgroundColor :: Color
    , _frames :: [Shape]
    }

-- Creates a new canvas, setting the size of canvas element
createCanvas :: Length -> Length -> Canvas
createCanvas width height = Canvas width height 24 Transparent []

-- Set the background color of the canvas
background :: Color -> Canvas -> Canvas
background color canvas = canvas{_backgroundColor = color}

-- Set the fps of the canvas
fps :: Int -> Canvas -> Canvas
fps fps canvas = canvas{_fps = fps}

-- Add a shape to the canvas
(<<<) :: Canvas -> Shape -> Canvas
(<<<) canvas shape = canvas{_frames = _frames canvas ++ [shape]}

-- Add a list of shapes to the canvas
(<<<:) :: Canvas -> [Shape] -> Canvas
(<<<:) canvas shapes = canvas{_frames = _frames canvas ++ shapes}

-- Set operator precedence
infixl 7 <<<
infixl 7 <<<:

-- This transformation requires the canvas as an input, so it is not included in the Shape module
center :: Canvas -> Shape -> Shape
center _ Empty = Empty
center canvas (Group shapes) = group shapes (center canvas)
center (Canvas w h _ _ _) line@(Line l opts) =
    line
        { _options =
            opts
                { _position =
                    Vector
                        ((w - l * cos (_angle opts)) / 2)
                        ((h - l * sin (_angle opts)) / 2)
                }
        }
center (Canvas w h _ _ _) rect@(Rect x y opts) =
    rect
        { _options =
            opts
                { _position =
                    Vector
                        (w / 2 - (x / 2 * cos (_angle opts) - y / 2 * sin (_angle opts)))
                        (h / 2 - (x / 2 * sin (_angle opts) + y / 2 * cos (_angle opts)))
                }
        }
center (Canvas w h _ _ _) curve@(Curve pts opts) =
    curve
        { _options =
            opts
                { _position =
                    Vector
                        ((w - mag (last pts) * cos (_angle opts + arg (last pts))) / 2)
                        ((h - mag (last pts) * sin (_angle opts + arg (last pts))) / 2)
                }
        }
center (Canvas w h _ _ _) shape =
    shape
        { _options =
            (_options shape)
                { _position = Vector (w / 2) (h / 2)
                }
        }
