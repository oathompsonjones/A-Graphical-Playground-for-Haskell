module Lib (
    module Color,
    module Maths,
    module Shape,
    Canvas (),
    render,
    createCanvas,
    background,
    fps,
    (<<<),
) where

import Color
import Internal ()
import Maths
import Shape (
    Shape,
    arc,
    bezier2,
    bezier3,
    circle,
    ellipse,
    empty,
    fill,
    line,
    noFill,
    noStroke,
    pie,
    polygon,
    rect,
    rotate,
    scale,
    segment,
    square,
    stroke,
    strokeWeight,
    translate,
    translateX,
    translateY,
    (&),
    (>>>),
 )

-- Data structure to represent a canvas
data Canvas = Canvas
    { _width :: Int
    , _height :: Int
    , _fps :: Int
    , _backgroundColor :: Color
    , _frames :: [Shape]
    }

-- Convert a canvas to a JSON string
instance Show Canvas where
    show :: Canvas -> String
    show (Canvas w h r b f) = "{\"w\":" ++ show w ++ ",\"h\":" ++ show h ++ ",\"r\":" ++ show r ++ ",\"b\":" ++ show b ++ ",\"f\":" ++ show f ++ "}"

-- Outputs the canvas in JSON format, wrapped in a function call
render :: Canvas -> IO ()
render canvas = putStrLn $ "drawToCanvas(" ++ show canvas ++ ")"

-- Creates a new canvas, setting the size of canvas element
createCanvas :: Int -> Int -> Canvas
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

-- Set operator precedence
infixl 7 <<<
