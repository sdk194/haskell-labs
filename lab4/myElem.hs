myElem :: Eq a => a -> [a] -> Bool

myElem n [] = False
myElem n (h:t) = if n == h 
  then True
  else myElem n t
