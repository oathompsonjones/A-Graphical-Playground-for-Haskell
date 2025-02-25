module Maths where

type Length = Float

----------------
---- Angles ----
----------------

type Radians = Float
type Degrees = Float

radians :: Degrees -> Radians
radians degrees = degrees * pi / 180

degrees :: Radians -> Degrees
degrees radians = radians * 180 / pi

-----------------
---- Vectors ----
-----------------

-- Data structure to represent vectors
data Vector = Vector Float Float deriving (Eq)

-- Vector maths
(^+^) :: Vector -> Vector -> Vector
(^+^) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

(^-^) :: Vector -> Vector -> Vector
(^-^) (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

(^*^) :: Vector -> Float -> Vector
(^*^) (Vector x y) s = Vector (x * s) (y * s)

(^/^) :: Vector -> Float -> Vector
(^/^) (Vector x y) s = Vector (x / s) (y / s)

mag :: Vector -> Float
mag (Vector x y) = sqrt (x ^ 2 + y ^ 2)

arg :: Vector -> Float
arg (Vector x y) = atan2 y x

norm :: Vector -> Vector
norm v = v ^/^ mag v

dot :: Vector -> Vector -> Float
dot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

cross :: Vector -> Vector -> Float
cross a b = mag a * mag b * sin (acos (dot a b))
