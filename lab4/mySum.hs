mySum :: Num a => [a] -> a

mySum [] = 0
mySum (h:t) = h + mySum t
