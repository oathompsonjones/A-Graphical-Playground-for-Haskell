import Data.Maybe

-- Data Types

-- Stores a 2D point
data Point = Point
    { x :: Float
    , y :: Float
    }

instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- Stores a color
data Color
    = RGBA
        { r :: Float
        , g :: Float
        , b :: Float
        , a :: Float
        }
    | RGB
        { r :: Float
        , g :: Float
        , b :: Float
        }

instance Show Color where
    show (RGBA r g b a) = "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ ")"
    show (RGB r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

-- Drawing to canvas

drawToCanvas :: String -> IO ()
drawToCanvas s = putStrLn $ "drawToCanvas(" ++ s ++ ")"

background :: Color -> IO ()
background c = drawToCanvas $ "background(" ++ show c ++ ")"

-- 2D Primitives (All arguments take the format of [shape specific arguments], (fillColour), (strokeColour), (strokeWeight))

circle :: Point -> Float -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
circle p r fc sc sw = drawToCanvas $ "circle(" ++ show p ++ ", " ++ show r ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

ellipse :: Point -> Float -> Float -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
ellipse p r1 r2 fc sc sw = drawToCanvas $ "ellipse(" ++ show p ++ ", " ++ show r1 ++ ", " ++ show r2 ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

line :: Point -> Point -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
line p1 p2 fc sc sw = drawToCanvas $ "line(" ++ show p1 ++ ", " ++ show p2 ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

point :: Point -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
point p fc sc sw = drawToCanvas $ "point(" ++ show p ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

quad :: Point -> Point -> Point -> Point -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
quad p1 p2 p3 p4 fc sc sw = drawToCanvas $ "quad(" ++ show p1 ++ ", " ++ show p2 ++ ", " ++ show p3 ++ ", " ++ show p4 ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

rect :: Point -> Float -> Float -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
rect p w h fc sc sw = drawToCanvas $ "rect(" ++ show p ++ ", " ++ show w ++ ", " ++ show h ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

square :: Point -> Float -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
square p s fc sc sw = drawToCanvas $ "square(" ++ show p ++ ", " ++ show s ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"

triangle :: Point -> Point -> Point -> Maybe Color -> Maybe Color -> Maybe Float -> IO ()
triangle p1 p2 p3 fc sc sw = drawToCanvas $ "triangle(" ++ show p1 ++ ", " ++ show p2 ++ ", " ++ show p3 ++ ", " ++ show fc ++ ", " ++ show sc ++ ", " ++ show sw ++ ")"
