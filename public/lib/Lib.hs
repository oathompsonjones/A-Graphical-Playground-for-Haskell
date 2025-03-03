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

import Canvas
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
render canvas = putStrLn $ "drawToCanvas(" ++ show canvas ++ ")"
