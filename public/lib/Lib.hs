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
data Canvas = Canvas {w :: Int, h :: Int, bg :: Color, ss :: [Shape]}

-- Convert a canvas to a JSON string
instance Show Canvas where
    show :: Canvas -> String
    show (Canvas w h bg ss) =
        "{ \"width\": "
            ++ show w
            ++ ", \"height\": "
            ++ show h
            ++ ", \"backgroundColor\": "
            ++ show bg
            ++ ", \"shapes\": "
            ++ show ss
            ++ " }"

-- Outputs the canvas in JSON format, wrapped in a function call
render :: Canvas -> IO ()
render c = putStrLn $ "drawToCanvas(" ++ show c ++ ")"

-- Creates a new canvas, setting the size of canvas element
createCanvas :: Int -> Int -> Canvas
createCanvas w h = Canvas w h Transparent []

-- Set the background color of the canvas
background :: Color -> Canvas -> Canvas
background cl cv = cv{bg = cl}

-- Add a shape to the canvas
(<<<) :: Canvas -> Shape -> Canvas
(<<<) c s = c{ss = ss c ++ [s]}

-- Individually modifies pixels in the canvas
setPixel :: Canvas -> Vector -> Color -> Canvas
setPixel cv pos cl = cv{ss = ss cv ++ [fill cl (stroke Transparent (translate pos (square 1)))]}

-- Set operator precedence
infixl 7 <<<