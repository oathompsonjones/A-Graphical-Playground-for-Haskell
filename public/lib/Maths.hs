module Maths where

import Data.Time.Clock.POSIX (getPOSIXTime)

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

infixl 6 ^+^
infixl 6 ^-^
infixl 7 ^*^
infixl 7 ^/^

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

----------------
---- Random ----
----------------

randoms :: Int -> [Double]
randoms seed = map fst (iterate (lcg . snd) (lcg seed))
  where
    lcg :: Int -> (Double, Int)
    lcg seed = (fromIntegral newSeed / fromIntegral (2 ^ 32), newSeed)
      where
        newSeed = (1664525 * seed + 1013904223) `mod` 2 ^ 32

seed :: IO Int
seed = do
    time <- getPOSIXTime
    return (floor (time * 1000000))
