module Canvas where

import Color
import Shape

-- Data structure to represent a canvas
data Canvas = Canvas
    { _width :: Int
    , _height :: Int
    , _fps :: Int
    , _backgroundColor :: Color
    , _frames :: [Shape]
    }

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

-- Add a list of shapes to the canvas
(<<<:) :: Canvas -> [Shape] -> Canvas
(<<<:) canvas shapes = canvas{_frames = _frames canvas ++ shapes}

-- Set operator precedence
infixl 7 <<<
infixl 7 <<<:
