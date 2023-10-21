myElem :: Eq a => a -> [a] -> Bool

myElem n [] = False
myElem n (h:t) = if n == h 
  then True
  else myElem n t

myIntersect :: Eq a => [a] -> [a] -> [a]

myIntersect [] _ = []
myIntersect (h1:t1) l = if myElem h1 l
  then [h1] ++ myIntersect t1 l
  else [] ++ myIntersect t1 l
