module Lib (
    module Color,
    module Maths,
    module Shape,
    Canvas (),
    render,
    createCanvas,
    background,
    (<<<),
    setPixel,
) where

import Color
import Maths
import Shape

-- Data structure to represent a canvas
data Canvas = Canvas {width :: Int, height :: Int, backgroundColor :: Color, shapes :: [Shape]}

-- Convert a canvas to a JSON string
instance Show Canvas where
    show :: Canvas -> String
    show (Canvas width height backgroundColor shapes) =
        "{ \"width\": "
            ++ show width
            ++ ", \"height\": "
            ++ show height
            ++ ", \"backgroundColor\": "
            ++ show backgroundColor
            ++ ", \"shapes\": "
            ++ show shapes
            ++ " }"

-- Outputs the canvas in JSON format, wrapped in a function call
render :: Canvas -> IO ()
render canvas = putStrLn $ "drawToCanvas(" ++ show canvas ++ ")"

-- Creates a new canvas, setting the size of canvas element
createCanvas :: Int -> Int -> Canvas
createCanvas width height = Canvas width height Transparent []

-- Set the background color of the canvas
background :: Color -> Canvas -> Canvas
background color canvas = canvas{backgroundColor = color}

-- Add a shape to the canvas
(<<<) :: Canvas -> Shape -> Canvas
(<<<) canvas shape = canvas{shapes = shapes canvas ++ [shape]}

-- Individually modifies pixels in the canvas
setPixel :: Canvas -> Vector -> Color -> Canvas
setPixel canvas position color = canvas{shapes = shapes canvas ++ [pixel]}
  where
    pixel = fill color (stroke Transparent (translate position (square 1)))
