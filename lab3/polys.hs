type Poly = [Float]

sumPoly :: Poly -> Poly -> Poly
sumPoly p [] = p
sumPoly [] p = p
sumPoly (x:xs) (y:ys) = [x + y] ++ sumPoly xs ys