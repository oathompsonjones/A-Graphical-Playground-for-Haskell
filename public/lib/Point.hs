module Point where

-- Data structure to represent points
data Point = Point {x :: Float, y :: Float}

-- Convert a point to a JSON string
instance Show Point where
    show (Point x y) = "{ \"x\": " ++ show x ++ ", \"y\": " ++ show y ++ " }"