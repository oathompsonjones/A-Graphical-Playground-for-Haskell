module Internal where

-- Remove ".0" from the end of a float
removeFloat :: Float -> String
removeFloat f
    | f == fromIntegral (round f) = show (round f)
    | otherwise = show f