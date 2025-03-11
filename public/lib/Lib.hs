module Lib (
    module Canvas,
    module Color,
    module CyclicCanvasShape,
    module Maths,
    module Shape,
    render,
) where

import Canvas
import Color
import CyclicCanvasShape
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
    regular,
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

-- Prints the instructions to render to the canvas
render :: Canvas -> IO ()
render canvas = do
    putStrLn $ "canvas(" ++ show canvas ++ ")"
    mapM_ (putStrLn . (\x -> "frame(" ++ show x ++ ")")) (_frames canvas)
    putStrLn "done()"
