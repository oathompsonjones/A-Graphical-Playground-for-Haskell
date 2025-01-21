module Maths where

----------------
---- Angles ----
----------------

radians :: Float -> Float
radians degrees = degrees * pi / 180

degrees :: Float -> Float
degrees radians = radians * 180 / pi

-----------------
---- Vectors ----
-----------------

-- Data structure to represent vectors
data Vector = Vector {x :: Float, y :: Float} deriving (Eq)

-- Convert a vector to a JSON string
instance Show Vector where
    show :: Vector -> String
    show (Vector x y) = "{ \"x\": " ++ show x ++ ", \"y\": " ++ show y ++ " }"

-- Vector maths
(<+>) :: Vector -> Vector -> Vector
(<+>) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

(<->) :: Vector -> Vector -> Vector
(<->) (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

(<*>) :: Vector -> Float -> Vector
(<*>) (Vector x y) scalar = Vector (x * scalar) (y * scalar)

(</>) :: Vector -> Float -> Vector
(</>) (Vector x y) scalar = Vector (x / scalar) (y / scalar)

mag :: Vector -> Float
mag (Vector x y) = sqrt (x ^ 2 + y ^ 2)

norm :: Vector -> Vector
norm vector = vector </> mag vector

dot :: Vector -> Vector -> Float
dot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

cross :: Vector -> Vector -> Float
cross a b = mag a * mag b * sin (acos (dot a b))

-- angle :: Vector -> Vector -> Float
-- angle a b = acos (dot a b / (mag a * mag b))