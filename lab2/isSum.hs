isSum :: Int -> Int -> Int -> Bool
isSum x y z = ((x + y) == z) == True || ((z + y) == x) == True || ((x + z) == y) == True