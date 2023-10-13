evalPoly :: Int -> [Int] -> Int
evalPoly _ [x] = x
evalPoly n (x:xs) = x + (n * (evalPoly n xs))