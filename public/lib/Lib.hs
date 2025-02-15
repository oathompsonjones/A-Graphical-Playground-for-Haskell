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
    setPixel,
) where

import Color
import Maths
import Shape

-- Data structure to represent a canvas
data Canvas = Canvas
    { _width :: Int
    , _height :: Int
    , _fps :: Int
    , _backgroundColor :: Color
    , _shapes :: [Shape]
    }

-- Convert a canvas to a JSON string
instance Show Canvas where
    show :: Canvas -> String
    show (Canvas w h f bg ss) = "{\"w\":" ++ show w ++ ",\"h\":" ++ show h ++ ",\"f\":" ++ show f ++ ",\"b\":" ++ show bg ++ ",\"s\":" ++ show ss ++ "}"

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
(<<<) canvas shape = canvas{_shapes = _shapes canvas ++ [shape]}

-- Individually modifies pixels in the canvas
setPixel :: Canvas -> Vector -> Color -> Canvas
setPixel canvas vector color = canvas{_shapes = _shapes canvas ++ [square 1 >>> fill color >>> stroke Transparent >>> translate vector]}

-- Set operator precedence
infixl 7 <<<