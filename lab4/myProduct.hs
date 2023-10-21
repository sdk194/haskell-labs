myProduct :: Num a => [a] -> a

myProduct [] = 0
myProduct [h] = h
myProduct (h:t) = h * myProduct t
