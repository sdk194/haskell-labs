len :: [[a]] -> Int
len [] = 0
len (x:_) = length x

shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
    then x
    else shortest xs