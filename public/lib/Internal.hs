module Internal where

-- Remove ".0" from the end of a float
removeFloat :: Float -> String
removeFloat float
    | float == fromIntegral (round float) = show (round float)
    | otherwise = show float