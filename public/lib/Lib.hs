module Lib (
    module Color,
    module Point,
    module Shape,
    render,
    createCanvas,
    background,
    appendShape,
    Canvas (),
) where

import Color
import Point
import Shape

data Canvas = Canvas {width :: Int, height :: Int, backgroundColor :: Color, shapes :: [Shape]}

instance Show Canvas where
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

render :: Canvas -> IO ()
render canvas = putStrLn $ "drawToCanvas(" ++ show canvas ++ ")"

createCanvas :: Int -> Int -> Canvas
createCanvas width height = Canvas width height Transparent []

background :: Canvas -> Color -> Canvas
background canvas color = canvas{backgroundColor = color}

appendShape :: Canvas -> Shape -> Canvas
appendShape canvas shape = canvas{shapes = shapes canvas ++ [shape]}
